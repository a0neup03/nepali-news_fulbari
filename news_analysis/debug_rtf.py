import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("./../manual_assignments.csv")

# 1) How many unique manual topics got docs?
print("Used manual topics:", df.manual_topic.nunique())

# 2) Bar chart of all topics
counts = df.manual_topic.value_counts()
counts.plot(kind="bar", figsize=(12,4))
plt.title("Docs per Manual Topic")
plt.rcParams["font.family"] = "DejaVu Sans"     # ships with Matplotlib and supports Devanagari
plt.rcParams["axes.unicode_minus"] = False      # in case you see weird minus signs
plt.show()

# 3) Pick a topic to inspect
topic = "राष्ट्रिय प्रजातन्त्र पार्टी"
sub = df[df.manual_topic==topic].sub_cluster.value_counts().sort_index()
print(topic, "sub-clusters:\n", sub)

# 4) Filter docs in sub-cluster ≥ 0 for that topic
print("Docs in real clusters:", df[(df.manual_topic==topic) & (df.sub_cluster>=0)].doc_id.tolist())

