# news_analysis/streamlit_app.py
import os
import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# ── CONFIG ──
BASE_DIR         = "/Users/neupanea2/Downloads/manifesto/nepali_news_dataset_20_categories_large"
ASSIGNMENTS_CSV  = "/Users/neupanea2/Downloads/manifesto/manual_assignments.csv"
SUBCLUSTERS_CSV  = "/Users/neupanea2/Downloads/manifesto/manual_subclusters.csv"
METRICS_CSV      = "/Users/neupanea2/Downloads/manifesto/topic_metrics.csv"
# ────────────

@st.cache_data
def load_data():
    assigns = pd.read_csv(ASSIGNMENTS_CSV)
    subs    = pd.read_csv(SUBCLUSTERS_CSV)
    metrics = pd.read_csv(METRICS_CSV)
    return assigns, subs, metrics

assigns, subs, metrics = load_data()

# create horizontal tabs
tab1, tab2 = st.tabs(["Browse by Topic", "Topic Metrics"])

with tab1:
    st.sidebar.title("Browse by Topic")
    topic = st.sidebar.selectbox("Pick a topic", sorted(assigns.manual_topic.unique()))

    sub_counts = subs[subs.manual_topic == topic][["sub_cluster","size"]]
    st.sidebar.table(sub_counts)

    search = st.sidebar.text_input("Keyword search in doc_id")

    df_topic = assigns[assigns.manual_topic == topic]
    if search:
        df_topic = df_topic[df_topic.doc_id.str.contains(search, case=False)]

    st.header(f"Articles tagged: {topic}")
    for _, row in df_topic.iterrows():
        st.markdown(f"**{row.doc_id}** — cluster {row.sub_cluster}")
        cat, fname = row.doc_id.split("/", 1)
        path = os.path.join(BASE_DIR, cat, fname)
        try:
            text = open(path, encoding="utf-8-sig", errors="ignore").read()
            snippet = text.replace("\n", " ")[:200] + "…"
        except FileNotFoundError:
            snippet = "(Could not load text file.)"
        st.write(snippet)
        st.write("---")

with tab2:
    st.header("Topic Confidence Metrics")

    # pick 10 topics you care about
    selected = [
        "ओली", "देउवा", "माओवादी केन्द्र", "एमाले",
        "चुनाव", "क्रिकेट", "अर्थतन्त्र",
        "नेपाली कांग्रेस", "भ्रष्टाचार", "फुटबल"
    ]
    sub = metrics[metrics.topic.isin(selected)].set_index("topic")[["doc_count","avg_sim"]]

    st.subheader("Counts & Average Similarity")
    st.dataframe(sub)

    # heatmap
    fig, ax = plt.subplots(figsize=(8, 4))
    mat = sub.values.T  # shape (2,10)
    im = ax.imshow(mat, aspect="auto", cmap="viridis")
    ax.set_xticks(np.arange(len(sub.index)))
    ax.set_xticklabels(sub.index, rotation=45, ha="right", fontfamily="DejaVu Sans")
    ax.set_yticks([0,1])
    ax.set_yticklabels(["doc_count","avg_sim"], fontfamily="DejaVu Sans")

    # annotate
    for i in range(mat.shape[0]):
        for j in range(mat.shape[1]):
            v = mat[i,j]
            txt = f"{int(v)}" if i==0 else f"{v:.2f}"
            ax.text(j, i, txt, ha="center", va="center", color="white", fontsize=8)

    fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    st.pyplot(fig)
