#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os, re, warnings
warnings.filterwarnings("ignore", category=FutureWarning, module="sklearn")

import pandas as pd
import numpy as np
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity
import hdbscan
from sklearn.feature_extraction.text import TfidfVectorizer

# ───────── CONFIG ─────────
BASE_DIR      = "/Users/neupanea2/Downloads/manifesto/nepali_news_dataset_20_categories_large"
SCRIPT_DIR    = "/Users/neupanea2/Downloads/manifesto"
TOPICS_FILE   = os.path.join(SCRIPT_DIR, "topics.txt")
SUFFIX_FILE   = os.path.join(SCRIPT_DIR, "nepali_suffixes.txt")
ALIASES_FILE = os.path.join(SCRIPT_DIR, "topic_aliases.txt")

STOP_FILE     = os.path.join(SCRIPT_DIR, "nepali_stopwords.txt")

EMBED_MODEL   = "/Users/neupanea2/Downloads/manifesto/models/paraphrase-multilingual-MiniLM-L12-v2"

SIM_THRESHOLD = 0.4    # cosine threshold for multi‐topic hits
MIN_CLUSTER   = 10      # HDBSCAN minimum cluster size
TRIAL_SIZE    = None    # None => load all documents
# ──────────────────────────

# ─── Load stopwords & suffixes ───
with open(SUFFIX_FILE, encoding="utf-8") as f:
    nepali_suffixes = [ln.strip() for ln in f if ln.strip()]
with open(STOP_FILE, encoding="utf-8") as f:
    nepali_stopwords = set(ln.strip() for ln in f if ln.strip())
# ──────────────────────────────────



def load_aliases(path):
    """Returns (alias_to_canonical, canonical_list)."""
    m = {}
    canonicals = []
    with open(path, encoding="utf-8-sig") as f:
        for ln in f:
            ln = ln.strip()
            if not ln or ln.startswith("#"): 
                continue
            key, vals = ln.split(":", 1)
            canon = key.strip()
            aliases = [v.strip() for v in vals.split(",")]
            # map the canonical to itself
            m[canon] = canon
            # and map each alias to the same canonical
            for a in aliases:
                if a:
                    m[a] = canon
            canonicals.append(canon)
    return m, canonicals




def load_topics(path):
    with open(path, encoding="utf-8-sig") as f:
        return [ln.strip() for ln in f if ln.strip()]

def load_corpus(base_dir, max_docs=None):
    recs, cnt = [], 0
    for cat in os.listdir(base_dir):
        cd = os.path.join(base_dir, cat)
        if not os.path.isdir(cd): continue
        for fn in os.listdir(cd):
            if not fn.endswith(".txt"): continue
            text = open(os.path.join(cd,fn),
                        encoding="utf-8-sig", errors="ignore").read()
            recs.append({"doc_id":f"{cat}/{fn}", "text":text})
            cnt += 1
            if max_docs and cnt >= max_docs:
                return pd.DataFrame(recs)
    return pd.DataFrame(recs)

def preprocess_nepali(text):
    t = re.sub(r"[\x00-\x1F\x7F-\x9F]", "", text)
    t = re.sub(r"[।॥,:!?\-\+\"'\(\)\[\]\{\}]", " ", t)
    t = t.lower().strip()
    t = re.sub(r"[^\u0900-\u097F\s]", " ", t)    
    t = re.sub(r"\s+"," ", t)
    ws = [w for w in t.split() if len(w)>1 and w not in nepali_stopwords]
    out = []
    for w in ws:
        for suf in sorted(nepali_suffixes, key=len, reverse=True):
            if w.endswith(suf) and len(w) > len(suf)+2:
                w = w[:-len(suf)]
                break
        out.append(w)
    return " ".join(out)

def assign_manual_topics_multi(df, topics, embedder, alias_map, threshold=SIM_THRESHOLD):
    # build the full list of variants (every alias and every canonical)
    variants = list(alias_map.keys())

    # embed once
    var_embs   = embedder.encode(variants, convert_to_numpy=True, show_progress_bar=False)
    doc_embs   = embedder.encode(df["text"].tolist(), convert_to_numpy=True, show_progress_bar=True)

    sims       = cosine_similarity(doc_embs, var_embs)

    multi = []
    for row in sims:
        # collect all variant hits ≥ threshold
        hits = [variants[i] for i, s in enumerate(row) if s >= threshold]
        if not hits:
            # fallback to best single variant
            best_i = int(np.argmax(row))
            hits = [variants[best_i]]

        # map variants → canonicals, dedupe
        canons = { alias_map[v] for v in hits if v in alias_map }
        multi.append(list(canons))

    df2 = df.copy()
    df2["manual_topics"] = multi
    return df2




def cluster_on_slice(df_slice, topic_name):
    # HDBSCAN on raw text embeddings
    embs   = embedder.encode(df_slice["text"].tolist(), convert_to_numpy=True, show_progress_bar=False)
    labels = hdbscan.HDBSCAN(
        min_cluster_size=MIN_CLUSTER,
        metric="euclidean",
        cluster_selection_method="eom"
    ).fit_predict(embs)

    # build per-doc assignments
    assign = pd.DataFrame({
        "doc_id":       df_slice["doc_id"],
        "manual_topic": topic_name,
        "sub_cluster":  labels
    })

    # TF-IDF labeling on the preprocessed text
    proc = df_slice["processed"].tolist()
    tfidf = TfidfVectorizer(
        max_features=2000,
        token_pattern=r'(?u)\b[\u0900-\u097F]{3,}\b')
    X     = tfidf.fit_transform(proc)
    terms = tfidf.get_feature_names_out()

    rows = []
    for lbl, cnt in zip(*np.unique(labels, return_counts=True)):
        if lbl < 0: 
            continue
        idxs = np.where(labels == lbl)[0]
        avg  = X[idxs].mean(axis=0).A1
        top5 = [terms[i] for i in avg.argsort()[-5:][::-1]]
        rows.append({
            "manual_topic": topic_name,
            "sub_cluster":  int(lbl),
            "size":         int(cnt),
            "label_terms":  ", ".join(top5)
        })

    return pd.DataFrame(rows), assign

def main():
    # 1) Load your alias map and canonical topic list
    print("Loading topic aliases…")
    alias_map, topics = load_aliases(ALIASES_FILE)
    print(f"Loaded {len(topics)} canonical topics from aliases file")

    # 2) Load & preprocess your corpus
    print("Loading corpus…")
    df = load_corpus(BASE_DIR, max_docs=TRIAL_SIZE)

    print("Preprocessing texts…")
    df["processed"] = df["text"].apply(preprocess_nepali)

    # 3) Initialize the embedding model
    print("Initializing embedding model…")
    global embedder
    embedder = SentenceTransformer(EMBED_MODEL, local_files_only=True)

    # 4) Assign manual topics (multi-label) using your alias_map
    print("Assigning manual topics (multi-label with aliases)…")
    df = assign_manual_topics_multi(df, topics, embedder, alias_map, threshold=SIM_THRESHOLD)

    # 5) Quick stats on coverage
    exploded = df.explode("manual_topics")
    print("\nTop 10 manual topics by doc count:")
    print(exploded["manual_topics"].value_counts().head(10))

    used = exploded["manual_topics"].nunique()
    print(f"\nUsed {used} of {len(topics)} manual topics.")
    missing = set(topics) - set(exploded["manual_topics"].unique())
    print("Topics with ZERO docs:", sorted(missing))

    # 6) Now sub-cluster each topic‐slice
    summaries, assignments = [], []
    for mt in topics:
        slice_df = df[df["manual_topics"].apply(lambda L: mt in L)]
        if len(slice_df) < MIN_CLUSTER:
            continue
        print(f"  ▶ Topic {mt!r} has {len(slice_df)} docs; clustering…")
        summ, asg = cluster_on_slice(slice_df, mt)
        summaries.append(summ)
        assignments.append(asg)

    # 7) Write out your CSVs
    if summaries:
        summary_df = pd.concat(summaries, ignore_index=True)
        assign_df  = pd.concat(assignments, ignore_index=True)
        summary_df.to_csv("manual_subclusters.csv", index=False, encoding="utf-8-sig")
        assign_df.to_csv( "manual_assignments.csv", index=False, encoding="utf-8-sig")
        print(f"\nWrote {len(summary_df)} manual sub-clusters")
    else:
        print("\nNo sub-clusters found (try lowering MIN_CLUSTER).")

if __name__ == "__main__":
    main()
