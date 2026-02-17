# ADR-015: Feature Signal Calibration via Percentile Normalization

## Status
Accepted

## Context
The ranking formula operates on 13 feature signals, each nominally [0,1]. In practice, different LLMs produce different distributions for the same signal. Qwen3-30B-A3B might output hype_score with mean 0.35 and standard deviation 0.12, while a future model might output mean 0.55 and standard deviation 0.25. The user's formula weights are tuned to one model's distribution. A model swap shifts the distribution, potentially inverting the meaning of weights: a weight of -0.3 on hype_score that suppresses hype under one model may be insufficient under another.

Candidates considered:
- **Raw scores (no normalization)**: Simplest. Model swap breaks weights. User must retune manually. Rejected — violates the transparency principle (feed behavior changes without user action).
- **Z-score normalization**: Compute z-score against running mean/std per signal, clip to [0,1]. Statistically principled. But: sensitive to outliers, requires Gaussian assumption (feature distributions are often bimodal or skewed), clipping loses information at tails. Rejected.
- **Calibration dataset**: Extract features for a fixed reference corpus with each model. Compute affine transform to align distributions. Most accurate. But: requires maintaining a reference corpus, increases model swap time, fragile if reference corpus doesn't represent user's content. Rejected for V1 (may revisit).
- **Percentile normalization**: After extraction, compute the item's percentile rank for each signal against the user's own recent item history. Signal value = percentile position [0,1]. Model-independent by construction — rank ordering is preserved regardless of raw score distribution.

## Decision
All 13 feature signals are percentile-normalized against the user's own item history before entering the scoring pipeline.

### Mechanism

For each signal, maintain a running histogram (100 bins, linearly spaced over [0,1]) per model. On each new item extraction:

1. Look up the item's raw signal value in the histogram to find its percentile rank.
2. The percentile rank (0.0 to 1.0) becomes the normalized signal value used by the scoring formula.
3. Insert the raw value into the histogram (histogram grows with each item).

### Storage

```sql
CREATE TABLE signal_histograms (
    model_id TEXT NOT NULL,
    signal_name TEXT NOT NULL,
    bin_index INTEGER NOT NULL,  -- 0-99
    count INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (model_id, signal_name, bin_index)
);
```

100 bins × 13 signals × 1 model = 1,300 rows. Negligible storage. Multiple models' histograms coexist — no data loss on model swap.

### Cold Start

The first 100 items use raw scores (histogram is too sparse for meaningful percentiles — at fewer than 100 items, percentile bins have fewer than 1 sample each on average, producing step-function artifacts). The system surfaces: "Calibrating ranking signals. Scores will stabilize after ~100 items." During cold start, raw [0,1] values pass through unchanged. After 100 items (hardcoded, not configurable), percentile normalization activates automatically. No user action required.

### Model Swap Behavior

On model swap:
1. New model gets a fresh histogram (empty).
2. As items are re-extracted with the new model, the new histogram fills.
3. During re-extraction, items use raw scores (new model cold start).
4. After ~100 re-extracted items, percentile normalization activates for the new model.
5. Old model's histogram is retained (for rollback).
6. User is notified: "Ranking recalibrating for new model. Expect shifts for ~100 items."

### Interpretation

Percentile normalization means: a hype_score of 0.8 means "this item is in the 80th percentile of hype among items you've seen." This is interpretable and model-independent. A weight of -0.3 on hype_score means "penalize items that are hype-ier than 70% of what I've seen" — this meaning is stable across model swaps.

### Content Maturity Exception

The `content_maturity` signal is NOT percentile-normalized. It is used as a hard pre-filter threshold for child profiles. Percentile normalization would make the threshold relative to the user's content history, which defeats the purpose — a child profile threshold of 0.3 should mean "block content above 0.3 maturity" absolutely, not "block the top 70% of mature content." Content maturity uses raw LLM scores with the conservative bias specified in story 011.

## Consequences
- **Positive**: Formula weights are stable across model swaps. Signals are interpretable as relative position. No reference dataset needed. Cheap to compute (histogram lookup). Graceful cold start.
- **Negative**: Percentile normalization is relative to the user's content mix — a user who only reads technical content has a different hype_score distribution than a user who reads tabloids. This is arguably a feature (calibrated to YOUR content), not a bug. First 100 items have uncalibrated scores. Content maturity cannot use this system.

## Constraints Implemented
- EC-041: Enrichment is idempotent (percentile lookup is deterministic given histogram state)
- OC-042: Single LLM call per item (calibration is post-processing, no additional LLM call)
