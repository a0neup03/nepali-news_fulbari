#!/usr/bin/env python3
# compute_topic_metrics.py

import os, re
import pandas as pd
import numpy as np
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity

# ─── CONFIG ───
BASE_DIR    = "nepali_news_dataset_20_categories_large"
TOPICS_FILE = "topics.txt"
EMBED_MODEL = "models/paraphrase-multilingual-MiniLM-L12-v2"
SIM_THRESH  = 0.30  # same threshold you used for multi-label
# ──────────────

# 1) load topics
topics = []
with open(TOPICS_FILE, encoding="utf-8-sig") as f:
    topics = [ln.strip() for ln in f if ln.strip()]

# 2) load corpus
docs = []
for cat in os.listdir(BASE_DIR):
    cd = os.path.join(BASE_DIR, cat)
    if not os.path.isdir(cd): continue
    for fn in os.listdir(cd):
        if not fn.endswith(".txt"): continue
        path = os.path.join(cd, fn)
        text = open(path, encoding="utf-8-sig", errors="ignore").read()
        docs.append({"doc_id": f"{cat}/{fn}", "text": text})
df = pd.DataFrame(docs)

# 3) embed
print("Embedding topics…")
embedder  = SentenceTransformer(EMBED_MODEL, local_files_only=True)
topic_em  = embedder.encode(topics, convert_to_numpy=True, show_progress_bar=False)
print("Embedding docs…")
doc_em    = embedder.encode(df["text"].tolist(), convert_to_numpy=True, show_progress_bar=True)

# 4) compute sims matrix
sims = cosine_similarity(doc_em, topic_em)

# 5) metrics for each topic
rows = []
for j, t in enumerate(topics):
    # docs with sim >= threshold
    hits = sims[:, j] >= SIM_THRESH
    count = int(hits.sum())
    avg_sim = float(sims[hits, j].mean()) if count>0 else 0.0
    rows.append({"topic": t, "doc_count": count, "avg_sim": avg_sim})

metrics = pd.DataFrame(rows)
metrics.to_csv("topic_metrics.csv", index=False, encoding="utf-8-sig")
print("Wrote topic_metrics.csv")
