// capture-user-turn.cpp — UserPromptSubmit hook: capture user prompts.
//
// Reads JSON from stdin (session_id, prompt), writes SHM files for mosaic/hooks,
// then produces a protobuf-serialized Turn to the chronicle.turns Kafka topic via librdkafka.
// The pipeline (TurnSearchConsumer) is the single owner of context_turns writes.
// Fire-and-forget: exits 0 on any error.
//
// Compile:
//   g++ -O2 -o capture-user-turn capture-user-turn.cpp $(pkg-config --cflags --libs rdkafka)

#include <librdkafka/rdkafka.h>
#include <cstdio>
#include <cstdint>
#include <cstring>
#include <ctime>
#include <fcntl.h>
#include <unistd.h>
#include <vector>

static constexpr int MAX_INPUT_SIZE = 8192;
static constexpr int MAX_PROMPT = 500;
static constexpr int UUID_BUF = 37; // 36 chars + null

// ─── Minimal protobuf encoder ───────────────────────────────────────────────
// Supports only what we need: varint, length-delimited (string/bytes/submessage).
// Wire types: 0 = varint, 2 = length-delimited.

static void pb_encode_varint(std::vector<uint8_t>& buf, uint64_t value) {
    while (value > 0x7F) {
        buf.push_back(static_cast<uint8_t>((value & 0x7F) | 0x80));
        value >>= 7;
    }
    buf.push_back(static_cast<uint8_t>(value));
}

static void pb_encode_tag(std::vector<uint8_t>& buf, int field, int wire_type) {
    pb_encode_varint(buf, static_cast<uint64_t>((field << 3) | wire_type));
}

// Field with varint value (int32, int64, enum).
static void pb_encode_varint_field(std::vector<uint8_t>& buf, int field, uint64_t value) {
    pb_encode_tag(buf, field, 0);
    pb_encode_varint(buf, value);
}

// Field with length-delimited value (string, bytes, embedded message).
static void pb_encode_bytes_field(std::vector<uint8_t>& buf, int field,
                                  const uint8_t* data, size_t len) {
    pb_encode_tag(buf, field, 2);
    pb_encode_varint(buf, len);
    buf.insert(buf.end(), data, data + len);
}

static void pb_encode_string_field(std::vector<uint8_t>& buf, int field,
                                   const char* str, size_t len) {
    pb_encode_bytes_field(buf, field, reinterpret_cast<const uint8_t*>(str), len);
}

// ─── Minimal JSON string extraction ─────────────────────────────────────────
// Finds "key":"value" and writes value into dst (up to dst_len-1 chars).
// Returns length written, or 0 if not found.
static int json_extract(const char* json, int json_len, const char* key, char* dst, int dst_len) {
    char pattern[128];
    int plen = snprintf(pattern, sizeof(pattern), "\"%s\":\"", key);
    if (plen <= 0 || plen >= (int)sizeof(pattern)) return 0;

    const char* pos = nullptr;
    const char* search = json;
    while ((pos = strstr(search, pattern)) != nullptr) {
        if (pos > json) {
            char prev = *(pos - 1);
            if (prev != '{' && prev != ',' && prev != ' ' && prev != '\t' && prev != '\n' && prev != '\r') {
                search = pos + 1;
                continue;
            }
        }
        break;
    }
    if (!pos) return 0;

    const char* start = pos + plen;
    const char* end = json + json_len;
    int i = 0;
    const char* p = start;
    while (p < end && i < dst_len - 1) {
        if (*p == '"') break;
        if (*p == '\\' && p + 1 < end) {
            p++;
            switch (*p) {
                case 'n': dst[i++] = '\n'; break;
                case 't': dst[i++] = '\t'; break;
                case 'r': dst[i++] = '\r'; break;
                case '"': dst[i++] = '"'; break;
                case '\\': dst[i++] = '\\'; break;
                case '/': dst[i++] = '/'; break;
                default: dst[i++] = *p; break;
            }
        } else {
            dst[i++] = *p;
        }
        p++;
    }
    dst[i] = '\0';
    return i;
}

// Generate UUID v4 from /dev/urandom. Returns false on failure.
static bool generate_uuid(char* buf) {
    unsigned char bytes[16];
    int fd = open("/dev/urandom", O_RDONLY);
    if (fd < 0) return false;
    ssize_t n = read(fd, bytes, 16);
    close(fd);
    if (n != 16) return false;

    bytes[6] = (bytes[6] & 0x0F) | 0x40;
    bytes[8] = (bytes[8] & 0x3F) | 0x80;

    snprintf(buf, UUID_BUF, "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
             bytes[0], bytes[1], bytes[2], bytes[3],
             bytes[4], bytes[5], bytes[6], bytes[7],
             bytes[8], bytes[9], bytes[10], bytes[11],
             bytes[12], bytes[13], bytes[14], bytes[15]);
    return true;
}

int main() {
    // Read stdin with raw POSIX I/O
    char input[MAX_INPUT_SIZE];
    int total = 0;
    while (total < MAX_INPUT_SIZE - 1) {
        ssize_t n = read(STDIN_FILENO, input + total, MAX_INPUT_SIZE - 1 - total);
        if (n <= 0) break;
        total += n;
    }
    input[total] = '\0';
    if (total == 0) return 0;

    // Extract fields
    char session_id[256];
    char prompt[MAX_INPUT_SIZE];
    int sid_len = json_extract(input, total, "session_id", session_id, sizeof(session_id));
    int prompt_len = json_extract(input, total, "prompt", prompt, sizeof(prompt));

    if (sid_len == 0) {
        strcpy(session_id, "unknown");
    }

    // Skip empty prompts
    if (prompt_len == 0 || (prompt_len == 4 && strcmp(prompt, "null") == 0)) {
        return 0;
    }

    // Truncate prompt to MAX_PROMPT chars
    if (prompt_len > MAX_PROMPT) {
        prompt[MAX_PROMPT] = '\0';
        prompt_len = MAX_PROMPT;
    }

    // Generate UUID
    char turn_id[UUID_BUF];
    if (!generate_uuid(turn_id)) return 0;

    // Write turn_id to SHM for mosaic to pick up
    int shm_fd = open("/dev/shm/mosaic_turn_id", O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (shm_fd >= 0) {
        write(shm_fd, turn_id, strlen(turn_id));
        close(shm_fd);
    }

    // Write session_id to SHM for mosaic to pick up
    if (sid_len > 0) {
        int shm_fd2 = open("/dev/shm/mosaic_session_id", O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (shm_fd2 >= 0) {
            write(shm_fd2, session_id, strlen(session_id));
            close(shm_fd2);
        }
    }

    // Capture wall-clock time once — used for SHM display and Kafka protobuf
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);

    // Generate human-readable CST timestamp for terminal display
    // Format: "Fri Feb 20, 2026 - 15:34:41:123 CST"
    {
        struct tm cst;
        time_t cst_time = ts.tv_sec - 6 * 3600;
        gmtime_r(&cst_time, &cst);
        int ms = (int)(ts.tv_nsec / 1000000);

        char display_ts[80];
        char date_part[48];
        strftime(date_part, sizeof(date_part), "%a %b %d, %Y - %H:%M:%S", &cst);
        snprintf(display_ts, sizeof(display_ts), "%s:%03d CST", date_part, ms);

        int ts_fd = open("/dev/shm/mosaic_timestamp", O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (ts_fd >= 0) {
            write(ts_fd, display_ts, strlen(display_ts));
            close(ts_fd);
        }
    }

    // ─── Produce to Kafka chronicle.turns (best-effort) ─────────────────────
    // TurnSearchConsumer is the single owner of context_turns writes.
    //
    // Build a protobuf-serialized Turn message matching turn.proto:
    //   field 1 (string): turn_id
    //   field 2 (string): session_id
    //   field 3 (int32):  pass_number = 1
    //   field 4 (enum):   role = ROLE_USER (1)
    //   field 5 (string): content
    //   field 7 (message): timestamp = google.protobuf.Timestamp { seconds, nanos }
    {
        // Encode google.protobuf.Timestamp submessage first
        std::vector<uint8_t> ts_msg;
        pb_encode_varint_field(ts_msg, 1, static_cast<uint64_t>(ts.tv_sec));   // seconds
        pb_encode_varint_field(ts_msg, 2, static_cast<uint64_t>(ts.tv_nsec));  // nanos

        // Encode the Turn message
        std::vector<uint8_t> turn_msg;
        turn_msg.reserve(512);
        pb_encode_string_field(turn_msg, 1, turn_id, strlen(turn_id));
        pb_encode_string_field(turn_msg, 2, session_id, strlen(session_id));
        pb_encode_varint_field(turn_msg, 3, 1);           // pass_number = 1
        pb_encode_varint_field(turn_msg, 4, 1);           // ROLE_USER = 1
        pb_encode_string_field(turn_msg, 5, prompt, prompt_len);
        // field 6 (repeated ToolCall) omitted — no tool calls for user turns
        pb_encode_bytes_field(turn_msg, 7, ts_msg.data(), ts_msg.size());  // Timestamp submessage

        // Create Kafka producer
        char errstr[256];
        rd_kafka_conf_t* conf = rd_kafka_conf_new();
        if (rd_kafka_conf_set(conf, "bootstrap.servers", "localhost:9092",
                              errstr, sizeof(errstr)) != RD_KAFKA_CONF_OK) {
            fprintf(stderr, "capture-user-turn: kafka conf error: %s\n", errstr);
            rd_kafka_conf_destroy(conf);
            return 0;
        }
        // Optimize for short-lived producer: small queue, fast linger
        rd_kafka_conf_set(conf, "queue.buffering.max.messages", "10", errstr, sizeof(errstr));
        rd_kafka_conf_set(conf, "linger.ms", "0", errstr, sizeof(errstr));

        rd_kafka_t* rk = rd_kafka_new(RD_KAFKA_PRODUCER, conf, errstr, sizeof(errstr));
        if (!rk) {
            fprintf(stderr, "capture-user-turn: kafka producer creation failed: %s\n", errstr);
            // conf is destroyed by rd_kafka_new on failure
            return 0;
        }

        // Produce the message
        rd_kafka_resp_err_t err = rd_kafka_producev(
            rk,
            RD_KAFKA_V_TOPIC("chronicle.turns"),
            RD_KAFKA_V_KEY(turn_id, strlen(turn_id)),
            RD_KAFKA_V_VALUE(turn_msg.data(), turn_msg.size()),
            RD_KAFKA_V_MSGFLAGS(RD_KAFKA_MSG_F_COPY),
            RD_KAFKA_V_END
        );
        if (err != RD_KAFKA_RESP_ERR_NO_ERROR) {
            fprintf(stderr, "capture-user-turn: kafka produce failed: %s\n",
                    rd_kafka_err2str(err));
        }

        // Flush with 1-second timeout to ensure delivery
        rd_kafka_flush(rk, 1000);

        rd_kafka_destroy(rk);
    }

    return 0;
}
