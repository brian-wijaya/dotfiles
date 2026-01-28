#!/home/bw/whisper-live/bin/python
"""
Fast real-time dictation using faster-whisper.
Toggle: run once to start, run again to stop.
"""
import os
import sys
import signal
import subprocess
import numpy as np
import sounddevice as sd
from pathlib import Path
from scipy import signal as scipy_signal

PID_FILE = "/tmp/whisper-dictate.pid"
MODEL_ID = "deepdml/faster-whisper-large-v3-turbo-ct2"
DEVICE_RATE = 44100  # MOTU M2 native rate
WHISPER_RATE = 16000  # Whisper expects 16kHz
CHUNK_MS = 500
CHUNK_SAMPLES = int(DEVICE_RATE * CHUNK_MS / 1000)
RESAMPLE_RATIO = WHISPER_RATE / DEVICE_RATE
DEVICE = 5  # M2: USB Audio (MOTU M2)

# Hallucination filter
HALLUCINATIONS = {
    "thank you", "thank you.", "thanks", "thanks.",
    "thanks for watching", "thanks for watching.",
    "subscribe", "bye", "bye.", "goodbye",
    "okay", "okay.", "ok", "ok.",
    "you", "the", "so", "um", "uh", "ah", "oh",
    "i'm sorry", "sorry",
    "...", "..", "."
}

RMS_THRESHOLD = 0.008  # Ignore quiet audio

def notify(msg):
    subprocess.run(["notify-send", "-t", "1000", msg], check=False)

def type_text(text):
    subprocess.run(["xdotool", "type", "--delay", "3", "--", text], check=False)

def is_hallucination(text):
    t = text.strip().lower()
    if len(t) < 3:
        return True
    if t in HALLUCINATIONS:
        return True
    # Check if starts with common hallucination
    for h in ["thank you", "thanks for", "subscribe", "bye"]:
        if t.startswith(h):
            return True
    # All punctuation/dots
    if all(c in ".,!?;:- " for c in t):
        return True
    return False

def toggle_check():
    """If already running, kill it and exit."""
    if Path(PID_FILE).exists():
        try:
            pid = int(Path(PID_FILE).read_text().strip())
            os.kill(pid, signal.SIGTERM)
        except (ProcessLookupError, ValueError):
            pass
        Path(PID_FILE).unlink(missing_ok=True)
        notify("Dictation OFF")
        sys.exit(0)

def cleanup(signum=None, frame=None):
    Path(PID_FILE).unlink(missing_ok=True)
    sys.exit(0)

def main():
    toggle_check()

    # Write PID
    Path(PID_FILE).write_text(str(os.getpid()))
    signal.signal(signal.SIGTERM, cleanup)
    signal.signal(signal.SIGINT, cleanup)

    notify("Dictation ON")

    # Load model
    from faster_whisper import WhisperModel
    model = WhisperModel(MODEL_ID, device="cuda", compute_type="float16")

    # Audio buffer (30 sec at device rate)
    buffer = np.zeros(DEVICE_RATE * 30, dtype=np.float32)
    write_idx = 0

    def audio_callback(indata, frames, time, status):
        nonlocal buffer, write_idx
        mono = indata[:, 0] if indata.ndim > 1 else indata.flatten()
        n = len(mono)
        end_idx = write_idx + n
        if end_idx <= len(buffer):
            buffer[write_idx:end_idx] = mono
        else:
            first = len(buffer) - write_idx
            buffer[write_idx:] = mono[:first]
            buffer[:n - first] = mono[first:]
        write_idx = end_idx % len(buffer)

    # Start audio stream at device native rate
    stream = sd.InputStream(
        samplerate=DEVICE_RATE,
        channels=1,
        dtype=np.float32,
        device=DEVICE,
        callback=audio_callback,
        blocksize=CHUNK_SAMPLES
    )

    import time

    with stream:
        speaking = False
        silence_count = 0
        speech_chunks = 0
        SILENCE_CHUNKS = 1  # Wait 1 chunk (500ms) of silence before transcribing

        while True:
            time.sleep(CHUNK_MS / 1000)

            # Check current audio level
            recent = np.roll(buffer, -write_idx)[-DEVICE_RATE // 2:]  # last 0.5s
            rms = np.sqrt(np.mean(recent ** 2))

            if rms >= RMS_THRESHOLD:
                if not speaking:
                    speaking = True
                    speech_chunks = 0
                speech_chunks += 1
                silence_count = 0
            elif speaking:
                silence_count += 1
                if silence_count >= SILENCE_CHUNKS:
                    # Speech ended - transcribe
                    # Grab audio for duration of speech + 1 sec buffer
                    speech_duration = (speech_chunks + silence_count + 2) * CHUNK_MS / 1000
                    samples_needed = int(DEVICE_RATE * min(speech_duration, 29))
                    audio_48k = np.roll(buffer, -write_idx)[-samples_needed:]

                    # Resample to 16kHz
                    num_samples = int(len(audio_48k) * RESAMPLE_RATIO)
                    audio = scipy_signal.resample(audio_48k, num_samples).astype(np.float32)

                    # Transcribe
                    segments, _ = model.transcribe(
                        audio,
                        language="en",
                        beam_size=1,
                        vad_filter=True,
                        vad_parameters=dict(min_silence_duration_ms=500),
                    )

                    text = "".join(s.text for s in segments).strip()

                    if text and not is_hallucination(text):
                        type_text(text + " ")

                    # Reset state but keep buffer (don't lose new audio)
                    speaking = False
                    silence_count = 0
                    speech_chunks = 0

if __name__ == "__main__":
    main()
