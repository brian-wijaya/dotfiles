#!/usr/bin/env python3
"""Generate elisp lemma hash table from LemmInflect/SPECIALIST Lexicon.

Data source: NIH SPECIALIST Lexicon via LemmInflect
- 1M+ lexical entries
- 96.1% verb accuracy (benchmarked)
- Comprehensive English language coverage

Usage:
    pip install lemminflect
    python generate-lemmas.py
"""

from pathlib import Path
import lemminflect
from lemminflect import getAllLemmas, getAllInflections

def extract_verb_lemmas():
    """Extract all verb inflection → lemma mappings from LemmInflect."""
    lemmas = {}

    # Use the Lemmatizer's internal dictionary which maps inflected -> lemma
    # Uses Universal Dependencies tags (VERB, NOUN, etc.)
    from lemminflect import Lemmatizer
    lem = Lemmatizer()
    lem_dict = lem._getLemmaDict()

    # Extract VERB entries
    for word, entry in lem_dict.items():
        if 'VERB' in entry:
            lemma = entry['VERB'][0]  # Take first lemma
            if word.lower() != lemma.lower():
                lemmas[word.lower()] = lemma.lower()

    return lemmas

def generate_elisp(lemma_dict, output_path):
    """Write elisp hash table."""
    with open(output_path, 'w') as f:
        f.write(';;; prog-speak-lemmas.el --- Verb lemma dictionary -*- lexical-binding: t; -*-\n\n')
        f.write(';; Generated from NIH SPECIALIST Lexicon via LemmInflect\n')
        f.write(';; Source: https://lhncbc.nlm.nih.gov/LSG/Projects/lexicon/current/web/index.html\n')
        f.write(';; DO NOT EDIT - regenerate with generate-lemmas.py\n\n')
        f.write(f'(defconst prog-speak--lemma-table\n')
        f.write(f'  (let ((ht (make-hash-table :test #\'equal :size {len(lemma_dict)})))\n')
        for word, lemma in sorted(lemma_dict.items()):
            # Escape any special characters in strings
            word_escaped = word.replace('\\', '\\\\').replace('"', '\\"')
            lemma_escaped = lemma.replace('\\', '\\\\').replace('"', '\\"')
            f.write(f'    (puthash "{word_escaped}" "{lemma_escaped}" ht)\n')
        f.write('    ht)\n')
        f.write('  "Hash table mapping inflected verbs to lemmas.")\n\n')
        f.write('(provide \'prog-speak-lemmas)\n')
        f.write(';;; prog-speak-lemmas.el ends here\n')

if __name__ == "__main__":
    print("Extracting verb lemmas from LemmInflect (SPECIALIST Lexicon)...")
    lemmas = extract_verb_lemmas()
    print(f"Found {len(lemmas)} verb inflection → lemma mappings")

    output = Path(__file__).parent.parent / "emacs" / "prog-speak-lemmas.el"
    output.parent.mkdir(parents=True, exist_ok=True)

    generate_elisp(lemmas, output)
    print(f"Wrote {output}")
