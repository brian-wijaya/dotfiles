---
name: btw-what-is
description: Explain concepts the user doesn't fully understand without polluting conversation context. Use when the user says "btw what is", "explain X to me", "what are crons", or any request to learn about a concept mid-conversation. Writes explanatory documents and opens them for reading.
argument-hint: <topic1> [topic2] [topic3]
---

# BTW What Is — Concept Explainer

Explain one or more concepts to the user by writing structured documents and opening them
for reading, WITHOUT polluting the current conversation context.

## Format Selection (use best judgment)

**HTML presentations** — default for conceptual learning:
- Architecture, protocols, system design (crons, webhooks, TCP/IP, etc.)
- Anything that benefits from diagrams, flowcharts, visual layout
- Historical/comparative explanations ("X vs Y")
- The user has praised HTML presentations with inline SVG as "world-class lectures"

**Org-mode in Emacs** — for hands-on practice topics:
- Coding patterns the user might want to type/practice in Emacs
- Mathematical formulas (LaTeX in org-mode)
- Elisp, org-mode features, Emacs workflows
- Configuration syntax (TOML, YAML, systemd units)
- Anything where the user benefits from having the content IN Emacs to edit/experiment with

**Both** — when a topic has a conceptual side AND a hands-on side:
- HTML for the visual explanation + org file for the practice material

## Procedure

1. **Parse topics** from the arguments. Multiple topics can be space-separated or comma-separated.

2. **Choose format** per topic using the judgment criteria above.

3. **Research each topic** using Task agents (subagent_type: general-purpose). Spawn one agent per topic in parallel. Each agent should:
   - Use the **pedagogical response protocol**: specification-grade rigor, dense nominal density, corrective behavior, curricular authority
   - Research the concept thoroughly (web search if needed for current info)
   - Write a deep, mechanically precise explanation aimed at an expert-adjacent learner
   - Focus on: what it is, why it matters, how it works mechanically, concrete examples, common misconceptions
   - Include comparisons to things the user likely already knows (Emacs, systemd, Linux, Java, Python, shell scripting)
   - Aim for a 10-15 minute read — thorough enough to be a standalone lecture, not a summary

4. **Write files** to `~/vault/org/btw/`:

   **For HTML presentations:**
   - Filename: `{topic-slug}.html`
   - Rich, dark-themed, single-page presentation with:
     - Inline SVG diagrams (not external files — everything self-contained)
     - Mermaid.js for flowcharts/sequence diagrams where appropriate
     - Clean typography: body >= 18px, headings scaled proportionally
     - Dark background (#1a1a2e or similar), light text (#e0e0e0)
     - Sections with clear visual hierarchy, generous whitespace
     - Code blocks with syntax-appropriate styling
     - Color-coded diagrams that reinforce the explanation
   - Style: lecture-quality. Think "the best explainer article you've ever read."
   - Font floor: 16px minimum for ANY text (user's 3440x1440 legibility rule)

   **For org-mode files:**
   - Filename: `{topic-slug}.org`
   - Structure:
     ```
     #+TITLE: {Topic Name}
     #+DATE: {today}
     #+FILETAGS: :btw:concept:{topic-slug}:

     * What It Is
     ...
     * How It Works
     ...
     * Try It Yourself
     (Exercises, code blocks the user can C-c C-c to execute)
     * How This Relates to Your System
     ```

5. **Open files on the user's display (:0)**:
   - HTML: use `ACT_execute_command` to open in Chrome
     - `google-chrome-stable --new-window "file:///path/to/file.html"`
   - Org files: use `ACT_evaluate_elisp` to open in Emacs
     - Open each file with `(find-file "path")`
     - Navigate from LAST to FIRST so user can use `next-buffer` to read in order
     - Final elisp: switch to the FIRST file's buffer
   - If BOTH formats for a topic: open HTML first, then org files

6. **Arrange windows** using `ACT_arrange_windows`:
   - Layout: chat terminal on left 1/3, content window(s) on right 2/3
   - i3 command: arrange the focused content window to take right side

7. **Respond briefly** in chat — ONLY the file paths. Example:
   "Created: ~/vault/org/btw/cron-jobs.html, ~/vault/org/btw/webhooks.html"
   Do NOT repeat, summarize, or preview the content in chat. The whole point is to keep the conversation clean.

## Rules

- NEVER dump explanatory content into the chat. That defeats the purpose.
- The ONLY chat output is the list of created file paths (absolute paths) once agents are done.
- If the user invokes this command, they ALWAYS want a full file-based explanation. Never "just answer in chat" — the act of invoking the command signals they want depth, not a quick answer.
- Use the **pedagogical response protocol** for all content: specification-grade rigor, dense nominal density, mechanical precision. The user is an expert-adjacent learner who wants to truly understand, not just get a summary.
- Connect every concept to the user's actual system where possible.
- Org files are automatically indexed by the vault watcher — they become searchable knowledge.
- Use the user's display (:0), NOT the agent display (:99).
- HTML presentations should be SELF-CONTAINED — inline all SVG, CSS, and JS. No external dependencies except CDN scripts (mermaid.js).
- Overwrite existing files — concepts evolve, always use fresh content.
