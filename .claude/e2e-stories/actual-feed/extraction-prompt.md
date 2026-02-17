# Stage 2 Extraction Prompt

## Overview

This is the single-call LLM prompt template for Stage 2 (Feature Extraction). Every content item passes through this prompt exactly once. The prompt produces structured JSON containing: summary, entities, claims, sources, topics, 8 LLM-scored feature signals, content maturity assessment, and tripwire evaluations.

**Model**: Qwen3-30B-A3B (primary), Qwen2.5-3B Q4 (fallback)
**Context window**: 32K tokens
**Output constraint**: JSON schema enforced by llama-server's `response_format.json_schema`

## Token Budget

| Component | Tokens (approx) |
|---|---|
| System prompt (static) | ~800 |
| User topics | ~100 |
| Tripwire conditions (up to 30) | ~4,500 |
| Content (title + body/transcript) | ~20,000 (max) |
| Output headroom | ~6,600 |
| **Total** | **~32,000** |

Content exceeding 20,000 tokens is truncated with a `[TRUNCATED]` marker. The prompt instructs the model to assess based on available content.

## System Prompt

```
You are a content analysis engine. You will receive a content item (article, video transcript, podcast transcript, or social media post) and produce a structured analysis.

Your analysis must be:
- Factual: assess what is present in the content, not what you wish were present
- Calibrated: scores should reflect genuine variation, not cluster around 0.5
- Conservative on maturity: when ambiguous, score maturity HIGHER (safer)
- Complete: every field must be populated, no nulls

You will also evaluate the content against a set of user-defined tripwire conditions. For each tripwire, assess whether the content matches the condition and provide a confidence score.
```

## User Message Template

```
<content>
Title: {{title}}
Author: {{author}}
Source: {{source_platform}} — {{source_url}}
Published: {{published_at}}
Modality: {{modality}}

{{body_or_transcript}}
</content>

<user_topics>
{{topics_csv}}
</user_topics>

<tripwires>
{{#each tripwires}}
[T{{index}}] {{condition_text}}
{{/each}}
</tripwires>

Analyze this content and produce a JSON response matching the required schema.
```

## Output JSON Schema

This schema is enforced by llama-server's `response_format.json_schema` — the model physically cannot produce non-conforming output.

```json
{
  "type": "object",
  "required": ["summary", "entities", "claims", "sources", "topics", "signals", "maturity", "tripwire_evaluations"],
  "properties": {
    "summary": {
      "type": "string",
      "description": "2-4 sentence summary of the content. Capture the central thesis and key conclusions. No editorializing."
    },
    "entities": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["name", "type"],
        "properties": {
          "name": { "type": "string" },
          "type": {
            "type": "string",
            "enum": ["person", "organization", "product", "technology", "location", "event", "concept"]
          }
        }
      },
      "description": "Named entities mentioned in the content. 5-20 entities typical."
    },
    "claims": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["statement", "type"],
        "properties": {
          "statement": {
            "type": "string",
            "description": "A single factual or analytical claim made in the content."
          },
          "type": {
            "type": "string",
            "enum": ["factual", "analytical", "predictive", "opinion"]
          }
        }
      },
      "description": "Key claims made in the content. 3-10 claims typical."
    },
    "sources": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["description", "type"],
        "properties": {
          "description": {
            "type": "string",
            "description": "What source is cited (e.g., 'peer-reviewed study in Nature', 'unnamed industry source')."
          },
          "type": {
            "type": "string",
            "enum": ["primary_research", "official_statement", "expert_opinion", "anonymous", "self_referential", "none"]
          }
        }
      },
      "description": "Sources cited within the content. Empty array if no sources cited."
    },
    "topics": {
      "type": "array",
      "items": { "type": "string" },
      "description": "3-8 topic tags. Lowercase, specific (e.g., 'transformer-architecture' not 'AI'). Match user_topics where applicable."
    },
    "signals": {
      "type": "object",
      "required": [
        "topic_relevance",
        "nominal_density",
        "credibility",
        "actionability",
        "hype_score",
        "vagueness",
        "repetition",
        "missing_sources"
      ],
      "properties": {
        "topic_relevance": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "How closely does this content match the user's declared topics? 0 = no overlap. 1 = direct, deep coverage of a declared topic."
        },
        "nominal_density": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "Ratio of specific, concrete, named referents (people, dates, quantities, product names, locations) to total content volume. 0 = entirely abstract/generic. 1 = maximally specific and referent-dense."
        },
        "credibility": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "How well-sourced and epistemically grounded is the content? Assess: named/traceable sources, cited evidence, clear methodology, hedging on uncertain claims, absence of logical fallacies. 0 = unsourced speculation. 1 = rigorous, multiply-sourced, transparent methodology."
        },
        "actionability": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "Can the reader do something concrete with this information? 0 = pure observation or opinion with no action path. 1 = specific, immediately executable guidance (steps, tools, commands, decisions)."
        },
        "hype_score": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "Degree of promotional, sensationalist, or engagement-bait language. Assess: superlatives without evidence, urgency language, clickbait patterns, unqualified claims of revolution/disruption. 0 = measured, proportionate language. 1 = maximally promotional or sensationalist."
        },
        "vagueness": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "Degree to which the content avoids commitment to specific claims. Assess: hedging, passive voice, unnamed sources, undefined terms, circular reasoning, weasel words. 0 = every claim is specific and falsifiable. 1 = entirely vague, no concrete commitments."
        },
        "repetition": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "How much of this content restates the same points, padded to fill length? 0 = every paragraph adds new information. 1 = single point restated throughout."
        },
        "missing_sources": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "Proportion of claims that should be sourced but are not. 0 = all claims that need sourcing have sources. 1 = major claims are entirely unsupported."
        }
      }
    },
    "maturity": {
      "type": "object",
      "required": ["overall", "categories", "reason"],
      "properties": {
        "overall": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "Overall content maturity score. 0 = suitable for all ages. 1 = explicitly adult. When ambiguous, score HIGHER (conservative). Calibration: 0.0-0.2 ≈ G/E rating, 0.2-0.5 ≈ PG-13/T, 0.5-0.7 ≈ R/M, 0.7-1.0 ≈ NC-17/AO."
        },
        "categories": {
          "type": "object",
          "required": ["violence", "sexual", "language", "substances", "graphic", "mature_themes"],
          "properties": {
            "violence": { "type": "number", "minimum": 0, "maximum": 1 },
            "sexual": { "type": "number", "minimum": 0, "maximum": 1 },
            "language": { "type": "number", "minimum": 0, "maximum": 1 },
            "substances": { "type": "number", "minimum": 0, "maximum": 1 },
            "graphic": { "type": "number", "minimum": 0, "maximum": 1 },
            "mature_themes": { "type": "number", "minimum": 0, "maximum": 1 }
          },
          "description": "Per-category maturity scores. Each 0-1, same calibration as overall."
        },
        "reason": {
          "type": "string",
          "description": "Brief explanation of why this maturity score was assigned. 1-2 sentences."
        }
      }
    },
    "tripwire_evaluations": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["index", "matches", "confidence", "excerpt"],
        "properties": {
          "index": {
            "type": "integer",
            "description": "The tripwire index [T0], [T1], etc."
          },
          "matches": {
            "type": "boolean",
            "description": "Does this content match the tripwire condition?"
          },
          "confidence": {
            "type": "number",
            "minimum": 0,
            "maximum": 1,
            "description": "Confidence in the match/non-match assessment."
          },
          "excerpt": {
            "type": "string",
            "description": "If matches=true, the specific text span that triggered the match. Empty string if matches=false."
          }
        }
      },
      "description": "One evaluation per tripwire in the input. Array length must equal number of tripwires."
    }
  }
}
```

## Signal Calibration Guidance

The extraction prompt produces 8 of the 13 total feature signals. The remaining 5 are computed post-LLM:

| Signal | Source | Computation |
|---|---|---|
| topic_relevance | LLM | Assessed against `<user_topics>` |
| nominal_density | LLM | Referent counting |
| credibility | LLM | Source/methodology assessment |
| actionability | LLM | Concrete guidance detection |
| hype_score | LLM | Promotional language detection |
| vagueness | LLM | Specificity assessment |
| repetition | LLM | Redundancy detection |
| missing_sources | LLM | Unsupported claim counting |
| source_authority | Post-LLM | User config lookup (exalted = 1.0, default = 0.5) |
| modality_preference | Post-LLM | User config lookup |
| freshness | Post-LLM | `1.0 / (1.0 + hours_since_published / 168.0)` |
| novelty | Post-LLM | `1.0 - max_cosine_similarity(embed, recent_100)` |
| content_maturity | LLM | `maturity.overall` (hard pre-filter, not ranked) |

All 8 LLM signals undergo percentile normalization against the user's own item history (100-bin histogram) after cold-start period (first 100 items use raw scores). Content maturity is exempt from normalization — it uses absolute scores because parental controls need deterministic thresholds.

## Example Output

For an article titled "PostgreSQL 18 Brings Native Vector Search":

```json
{
  "summary": "PostgreSQL 18 adds built-in vector similarity search via a new pgvector-compatible index type. Benchmarks show 3x faster ANN queries than the pgvector extension on datasets over 1M vectors. The feature is available in beta and expected to ship in the October 2026 release.",
  "entities": [
    {"name": "PostgreSQL 18", "type": "product"},
    {"name": "pgvector", "type": "technology"},
    {"name": "Andrew Dunstan", "type": "person"},
    {"name": "EDB", "type": "organization"}
  ],
  "claims": [
    {"statement": "PostgreSQL 18 includes native vector similarity search without extensions", "type": "factual"},
    {"statement": "Native implementation benchmarks at 3x faster than pgvector for >1M vector datasets", "type": "factual"},
    {"statement": "Feature will ship in the October 2026 release", "type": "predictive"}
  ],
  "sources": [
    {"description": "PostgreSQL 18 release notes (commit log reference)", "type": "primary_research"},
    {"description": "Benchmark results from EDB engineering team", "type": "official_statement"}
  ],
  "topics": ["postgresql", "vector-search", "database-internals", "approximate-nearest-neighbor"],
  "signals": {
    "topic_relevance": 0.85,
    "nominal_density": 0.78,
    "credibility": 0.82,
    "actionability": 0.45,
    "hype_score": 0.15,
    "vagueness": 0.12,
    "repetition": 0.08,
    "missing_sources": 0.20
  },
  "maturity": {
    "overall": 0.0,
    "categories": {
      "violence": 0.0,
      "sexual": 0.0,
      "language": 0.0,
      "substances": 0.0,
      "graphic": 0.0,
      "mature_themes": 0.0
    },
    "reason": "Technical database article with no mature content."
  },
  "tripwire_evaluations": [
    {"index": 0, "matches": true, "confidence": 0.92, "excerpt": "PostgreSQL 18 adds built-in vector similarity search via a new pgvector-compatible index type"},
    {"index": 1, "matches": false, "confidence": 0.05, "excerpt": ""}
  ]
}
```

## Prompt Engineering Notes

1. **Schema enforcement is the primary validation**. llama-server's `json_schema` constraint guarantees structural validity. The prompt text guides quality, not structure.

2. **Signal descriptions are the most load-bearing text in the system**. Each signal description must be unambiguous enough that two independent LLMs would produce scores within ±0.15 on the same content. The descriptions above are calibrated for this.

3. **Tripwire evaluation scales linearly**. At 30 tripwires (~4,500 tokens), the model has ~20K tokens left for content. At 0 tripwires, content can use ~25K tokens. The pipeline truncates content to fit, never tripwires.

4. **No chain-of-thought**. The model produces JSON directly. CoT would consume output tokens and slow inference. The structured schema already forces the model to "think" through each field sequentially.

5. **User topics seed relevance scoring**. Without topics, `topic_relevance` defaults to a generic assessment. The more specific the topics, the sharper the discrimination.

6. **Maturity conservative bias** is achieved by explicit instruction + calibration guidance in the schema description. This is the one signal where false positives are strictly preferred over false negatives.

## Versioning

The extraction prompt is versioned alongside the model. `extraction_model_id` in `feed_items` stores `{model_name}:{prompt_version}` (e.g., `qwen3-30b-a3b:v1`). Prompt version changes trigger re-extraction of recent items (configurable: last 7 days or last 500 items, whichever is smaller).

## Referenced By

- Implementation Plan: Task 2.4 (Stage 2 extraction pipeline)
- ADR-006: LLM extraction architecture
- Story 005: FeedItem schema and extraction
- Story 016: Error recovery (extraction failures)
