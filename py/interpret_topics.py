# ============================================================================ #
#                                ---- Setup ----
# ============================================================================ #

import ollama
import pandas as pd
import tqdm
import json
import py.ollama_utils  # Script to start/stop Ollama in the background

# ============================================================================ #
#                         ---- Read & prepare data ----
# ============================================================================ #

# Read the data with the topic top terms
dat = pd.read_csv(
    "data/top_terms_snsf_grants_ai_2011_2025_k_025.csv",
    sep=",",
    index_col="topic",
)

# Read the system and user prompts for the interpretation of topic top terms
with open("py/sys_prompt.txt", encoding="utf-8") as f:
    system_prompt = f.read()
with open("py/user_prompt.txt", encoding="utf-8") as f:
    user_prompt = f.read()

# Create vetor that will be populated with a label describing the topics
labels = ["" for i in range(len(dat.index.unique()))]
descriptions = ["" for i in range(len(dat.index.unique()))]

# ============================================================================ #
#                              ---- Query LLM ----
# ============================================================================ #

# Start Ollama process in the background
py.ollama_utils.start_ollama()

# Loop over each topuc and prompt Qwen 3 to interpret the topic
for i in tqdm.trange(len(dat.index.unique())):
    res = ollama.chat(
        model="qwen3:4b-instruct",
        options={"temperature": 0.4},
        messages=[
            {"role": "system", "content": system_prompt},
            {
                "role": "user",
                "content": user_prompt
                + "\n"
                + "\n".join(dat.loc[i + 1, "term"]),
            },
        ],
        format="",
    )
    json_res = json.loads(res.message.content)
    labels[i] = json_res["label"]
    descriptions[i] = json_res["description"]

# Add the topic label to the original data frame
labelled_dat = pd.DataFrame(
    {"label": labels, "description": descriptions}, index=dat.index.unique()
)
# Save the data with the topic top terms completed with a topic label
labelled_dat.to_csv(
    "data/top_terms_snsf_grants_ai_2011_2025_k_025_with_labels.csv", sep=","
)

# Stop Ollama
py.ollama_utils.stop_ollama()
