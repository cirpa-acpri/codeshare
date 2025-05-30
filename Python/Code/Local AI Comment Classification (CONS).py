"""
This script performs thematic analysis and classification of open-ended survey comments
using a local LLM API - currently configured to work with LM Studio, but could be hotwired
to work with other platforms. 

It loads raw comments from a CSV, optionally generates topics via batch summarization prompts 
to the LLM, and then labels each comment by assigning one or more topic codes to each comment.
The code also logs processing details for both topic generation and comment labeling.

TO USE: Go to the "Usage" section at the bottom.
1. Update the `question`, `file_path`.
    > You can also update the names of the log file or save directory
2. Specify where topics are coming from - you have three options, as described in the usage section.
3. (Optionally) Edit the PROMPTS in the designated section below.
4. Run the script. Outputs (logs and final CSVs) will be saved under the `saved_data` folder.

Recommend looking up, or testing out to find good settings for the models you're going to use
for the task at hand. If you're wanting to compare these quickly to your previous human-coding,
Fraser may have an R script for that, feel free to connect.

Good luck. May your AI guess well, and your margins of error be low.

-----------------
May, 2025
Credits: 
    Karamjot Singh (CONS - Workstudy) [karampruthi@conestogac.on.ca] - Key Architect
    Fraser Hay (CONS) [fhay@conestogac.on.ca] - Tester Extraordinaire
    ChatGPT - Cheif Code Writer, after Karam
"""

import os
import json
import csv
import time
import pandas as pd
import requests
import re
from datetime import datetime, timedelta

# ================================
# CONFIGURATION
# ================================

LM_SERVER_URL = "http://127.0.0.1:1234/v1/chat/completions"
timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

# Where all your outputs will go:
SAVE_DIR = os.path.join(os.getcwd(), "saved_data")
os.makedirs(SAVE_DIR, exist_ok=True)
topic_log_file = os.path.join(SAVE_DIR, f"topic_process_log_{timestamp}.csv")
label_log_file = os.path.join(SAVE_DIR, f"label_process_log_{timestamp}.csv")
CSV_FILE       = os.path.join(SAVE_DIR, f"final_labeled_comments_{timestamp}.csv")


# ================================
# PROMPTS
# ================================

LABEL_COMMENT_PROMPT = (
    "You are classifying comments submitted by students on a College feedback survey.\n\n"
    "Students were asked the following question:\n\"{question}\"\n\n"
    "Here is a response comment:\n\n'''{comment}'''\n\n"
    "For that comment, return a JSON object with a single key \"coding\", whose value is an array of one or more of topics from the list below that best represent the comment. "
    "Use as few topic codes as needed to capture the main idea(s) of the comment. "
    "Output only in JSON format, using only the topic titles, with no extra text. "
    "If no category is applicable, you can return a blank \"coding\" array.\n\n"
    "List of topic codes (with descriptions):\n"
    "{topic_prompt_str}\n\n"
)

TOPIC_BASE_INTRO = (
    "You are assisting in the thematic analysis of survey data. Specifically, you are "
    "looking at student answers to the following question:\n\"{question}\"\n\n"
)

TOPIC_BATCH_PROMPT = TOPIC_BASE_INTRO + (
    "Use the responses below to generate a list of unique topics (up to a maximum of {batch_topics_max}) with no conceptual overlap, capturing recurring themes, sentiments, and key ideas. "
    "Topics should only be generated if they directly relate to more than one comment. Each topic should be broad and general and include a short description (up to 150 characters) that explains the theme. "
    "Ignore comments that do not answer the question or are confusing.\n\n"
    "Output a JSON object with a single key \"topics\" whose value is an array of objects, each having a \"title\" and a \"description\".\n\n"
    "Comments:\n\n{comments}"
)

TOPIC_GENERALIZATION_PROMPT = TOPIC_BASE_INTRO + (
    "Below are the aggregated topics (each with a title and description) derived from survey responses.\n\n"
    "Condense these topics into a smaller number of overarching themes, ensuring they are "
    "distinct and non-overlapping. Compound categories (e.g., A & B) are allowed when the concepts have a logical relation.\n\n"
    "Output no more than {topic_goal} topics in JSON format (with key \"topics\"). Each topic should "
    "have a 'title' and a short 'description' (up to 150 characters). Ensure that the final topic is "
    "'N/A' with description 'Catch-all category for a response that does not fit any category, is confusing, or does not answer the question.'\n\n"
    "Aggregated Topics (in JSON):\n\n{aggregated}"
)

TOPIC_GENERALIZATION_NEW_BATCH_PROMPT = TOPIC_BASE_INTRO + (
    "Below are the current baseline topics (each with a title and description) and new topics from recent responses.\n\n"
    "Revise the baseline topics as needed (if required) to integrate the new topics under them, and consolidate them into a smaller number "
    "of overarching themes, with no conceptual overlap. If needed, broaden a topic to be more general. "
    "Compound categories (e.g., A & B) are allowed when the concepts relate logically.\n\n"
    "Important: Output fewer than (or up to) {topic_goal} topics in JSON format (with key \"topics\"). Each topic should have "
    "a 'title' and a short 'description' (up to 150 characters). Ensure that the final topic is 'N/A' with description "
    "'Catch-all category for a response that does not fit any category, is confusing, or does not answer the question.'\n\n"
    "Current Baseline Topics (in JSON):\n\n{baseline}\n\n"
    "New Batch Topics (in JSON):\n\n{new}"
)

# ================================
# UTILITY FUNCTIONS
# ================================

def extract_json_content(text):
    """
    Extract the first well-formed JSON object from a text blob.
    This function scans the input text for the first '{' character, then
    tracks nested braces to return the complete JSON substring. If no
    matching closing brace is found, returns the text from the first '{'
    to the end.

    Parameters:
        text (str): The raw response text potentially containing JSON.

    Returns:
        str: A substring that starts with '{' and ends at the matching '}',
             trimmed of leading/trailing whitespace.

    Notes:
        - If no '{' is found, returns the original text unchanged.
        - Useful for cleaning LLM responses that include extraneous messages
          before or after the JSON payload.
    """
    start = text.find('{')
    if start == -1:
        return text
    count = 0
    for i in range(start, len(text)):
        if text[i] == '{':
            count += 1
        elif text[i] == '}':
            count -= 1
            if count == 0:
                return text[start:i+1].strip()
    return text[start:].strip()


def append_to_csv(case, comment, codes, csv_file):
    """
    Append one or more coding results for a single comment to a CSV file.
    Opens (or creates) the specified CSV file in append mode, writing
    a header row if the file did not previously exist. For each code in
    `codes`, writes a row containing the case index, comment text, and code.

    Parameters:
        case (int): Sequential identifier for the comment (1-based).
        comment (str): The original comment text.
        codes (list of str): Topic codes assigned to this comment.
        csv_file (str): File path of the CSV file to append to.

    Notes:
        - If `codes` is empty or None, writes a single row with an empty code.
        - Ensures UTF-8 encoding and proper newline handling.
    """
    file_exists = os.path.exists(csv_file)
    with open(csv_file, mode="a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        if not file_exists:
            writer.writerow(["Case", "Comment", "Code"])
        if codes:
            for code in codes:
                writer.writerow([case, comment, code])
        else:
            writer.writerow([case, comment, ""])


def append_to_label_log(case, comment, success, attempt, temperature, duration, model, pt, ct, topic_count):
    """
    Record logging metrics for each comment labeling attempt.
    Appends a row to the label log CSV capturing performance and usage stats.

    Parameters:
        case (int): Sequential identifier for the comment (1-based).
        comment (str): The comment text being labeled.
        success (bool): Whether labeling ultimately succeeded.
        attempt (int): Number of attempts made for this comment.
        temperature (float): Final temperature used for the LLM.
        duration (float): Total elapsed time in seconds.
        model (str): Identifier of the LLM model used.
        pt (int): Number of prompt tokens sent.
        ct (int): Number of completion tokens received.
        topic_count (int): Number of topic codes assigned.

    Returns:
        None

    Notes:
        - Creates the log file if it doesn't exist, with an appropriate header.
        - Encodes all numeric metrics to standard formats.
    """
    file_exists = os.path.exists(label_log_file)
    with open(label_log_file, mode="a", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        if not file_exists:
            writer.writerow([
                "Case", "Comment", "Success", "Attempts",
                "FinalTemperature", "Duration_s", "Model", "PromptTokens", "CompletionTokens", "TopicCount"
            ])
        writer.writerow([
            case, comment, success, attempt,
            f"{temperature:.2f}", f"{duration:.2f}", model or "",
            pt, ct, topic_count
        ])


def save_json(data, filename):
    """
    Serialize a Python object to a JSON file under the SAVE_DIR directory.

    Parameters:
        data (Any): The Python object to serialize (e.g., list, dict).
        filename (str): Name of the output JSON file (no path).

    Notes:
        - Constructs the full path as os.path.join(SAVE_DIR, filename).
        - Writes with ensure_ascii=False and indent=2 for readability.
    """
    filepath = os.path.join(SAVE_DIR, filename)
    with open(filepath, "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=2)


def query_llm(prompt, temperature=0.2, json_schema=None):
    """
    Send a chat completion request to the local LLM server and return results.
    Writes the last prompt to disk, then posts a JSON payload to LM_SERVER_URL.
    Optionally enforces `json_schema` to validate the response structure.

    Parameters:
        prompt (str): The full prompt text to send to the model.
        temperature (float): Sampling temperature (0.0–1.0) for creativity.
        json_schema (dict, optional): JSON schema for response validation.

    Returns:
        tuple:
            content (str): The raw content of the model's response.
            total_tokens (int): Sum of prompt and completion tokens used.
            prompt_tokens (int): Number of tokens in the prompt portion.
            completion_tokens (int): Number of tokens in the completion.
            model (str): Name or identifier of the LLM model used.
    """
    last_prompt_path = os.path.join(SAVE_DIR, "last_prompt.txt")
    with open(last_prompt_path, "w", encoding="utf-8") as f:
        f.write(prompt)
    headers = {"Content-Type": "application/json"}
    payload = {
        "messages": [{"role": "user", "content": prompt}],
        "stream": False,
        "temperature": temperature
    }
    if json_schema is not None:
        payload["json_schema"] = json_schema
    response = requests.post(LM_SERVER_URL, headers=headers, data=json.dumps(payload))
    response.raise_for_status()
    data = response.json()
    content = data["choices"][0]["message"]["content"].strip()
    usage = data.get("usage", {})
    pt = usage.get("prompt_tokens", len(prompt.split()))
    ct = usage.get("completion_tokens", 0)
    model = data.get("model")
    return content, pt + ct, pt, ct, model


def load_topics(source=None):
    """
    Load topic definitions from a JSON file path or in-memory list.
    Supports three formats:
     - JSON file containing a list of dicts with 'title' and 'description'
     - JSON file containing a list of strings (topic titles)
     - Python list of strings

    Parameters:
        source (str or list, optional): Path to JSON file or list of topics.
            If None, returns None to signal that topics should be generated.

    Returns:
        list: Normalized list of topics, each either a dict with keys
              'title' and 'description', or a stripped string.
    """
    if source is None:
        return None
    if isinstance(source, str):
        with open(source, "r", encoding="utf-8-sig") as f:
            data = json.load(f)
        if not isinstance(data, list):
            raise ValueError("Topics JSON must be a list")
        if data and isinstance(data[0], dict):
            return [
                {"title": t.get("title", "").strip(), "description": t.get("description", "").strip()}
                for t in data
            ]
        elif data and isinstance(data[0], str):
            return [t.strip() for t in data]
        else:
            return data
    if isinstance(source, list) and all(isinstance(t, str) for t in source):
        return [t.strip() for t in source]
    raise ValueError("load_topics: unsupported source type")


# ================================
# CORE FUNCTIONS
# ================================

def load_comments(file_path, comment_column="Text"):
    """
    Read comments from a CSV file and save them to disk as JSON.
    Attempts UTF-8 decoding first; falls back to Latin-1 on failure.
    Extracts the specified column, drops empty values, and serializes.

    Parameters:
        file_path (str): Path to the CSV file containing survey data.
        comment_column (str): Name of the column with comment text.

    Returns:
        list of str: List of comment strings extracted from the CSV.

    Notes:
        - Saves the raw list to 'raw_comments.json' under SAVE_DIR.
        - Ensures that missing or NaN entries are treated as empty strings.
    """
    try:
        df = pd.read_csv(file_path, encoding="utf-8", keep_default_na=False)
    except UnicodeDecodeError:
        df = pd.read_csv(file_path, encoding="latin-1", keep_default_na=False)
    comments = df[comment_column].dropna().astype(str).tolist()
    save_json(comments, "raw_comments.json")
    return comments


def label_comments(
    comments, question, topics,
    start_index=0, max_retries=5,
    temperature=0.28, temp_increase=0.15
):
    """
    Iteratively classify survey comments by calling the LLM and record results.

    For each comment in `comments` starting at `start_index`, constructs
    a prompt using the survey `question` and predefined `topics`, then
    calls `query_llm`. Retries up to `max_retries` on failure, increasing
    temperature by `temp_increase` each attempt. On success, appends
    codes to CSV and logs metrics.

    Parameters:
        comments (list of str): List of comment texts to label.
        question (str): Survey question for prompt context.
        topics (list of dict or list of str): Predefined topics.
        start_index (int): Zero-based index to resume processing.
        max_retries (int): Maximum LLM retry attempts per comment.
        temperature (float): Initial LLM temperature setting.
        temp_increase (float): Temperature increment after each failure.

    Notes:
        - Writes labeled results to CSV_FILE.
        - Logs detailed attempt metrics via `append_to_label_log`.
        - Prints progress, token usage, timing, and ETA to stdout.
    """
    # Prepare topics JSON for prompt
    if topics and isinstance(topics[0], dict):
        topic_data = {"topics": topics}
    else:
        topic_data = {"topics": [{"title": t, "description": ""} for t in topics]}
    topic_prompt_str = json.dumps(topic_data, indent=2)

    json_schema = {
        "name": "code_schema",
        "schema": {
            "type": "object",
            "properties": {
                "coding": {"type": "array", "items": {"type": "string"}}
            },
            "required": ["coding"],
            "additionalProperties": False
        }
    }

    total = len(comments)
    total_success_time = 0.0
    success_count = 0

    for i in range(start_index, total):
        case_num = i + 1
        comment = comments[i]

        # Initialize logging variables
        attempt = 0
        success = False
        elapsed = 0.0
        model = None
        pt = 0
        ct = 0
        topic_count = 0
        current_temp = temperature
        start_time = time.time()

        prompt = LABEL_COMMENT_PROMPT.format(
            question=question,
            comment=comment,
            topic_prompt_str=topic_prompt_str
        )

        # Retry loop
        while attempt < max_retries and not success:
            attempt += 1
            try:
                response_text, total_tokens, p_tokens, c_tokens, used_model = query_llm(
                    prompt, temperature=current_temp, json_schema=json_schema
                )
                if not response_text.strip():
                    raise ValueError("Empty response from LLM.")
                cleaned = extract_json_content(response_text)
                parsed = json.loads(cleaned)
                codes = parsed.get("coding", [])
                valid = [t["title"] for t in topic_data["topics"]]
                invalid = [c for c in codes if c not in valid]
                if invalid:
                    raise ValueError(f"Invalid codes returned: {invalid}")

                append_to_csv(case_num, comment, codes, CSV_FILE)

                # Metrics on success
                elapsed = time.time() - start_time
                topic_count = len(codes)
                model = used_model
                pt = p_tokens
                ct = c_tokens

                success_count += 1
                total_success_time += elapsed

                avg_time = total_success_time / success_count
                rem = total - case_num
                eta = timedelta(seconds=int(rem * avg_time))
                eta_str = f"{eta.seconds//3600}h{(eta.seconds%3600)//60}m"
                pct = (case_num / total) * 100
                print(
                    f"Processed {case_num}/{total} ({pct:.1f}%) - "
                    f"Tokens: {pt} in, {ct} out - "
                    f"{elapsed:.1f}s (avg {avg_time:.1f}s; ETA {eta_str})."
                )

                success = True

            except Exception as e:
                pct = (case_num / total) * 100
                elapsed = time.time() - start_time
                print(
                    f"Error {case_num}/{total} ({pct:.1f}%) attempt [{attempt}] "
                    f"in {elapsed:.1f}s: {e}"
                )
                try:
                    print(f"Response received: {response_text}")
                except NameError:
                    pass
                current_temp = min(current_temp + temp_increase, 1.0)
                print(f"Increasing temperature to: {current_temp:.2f}")
                if attempt >= max_retries:
                    print(f"Failed to process case {case_num} after {max_retries} attempts. Moving on.")

        # One log entry per case
        total_duration = time.time() - start_time
        append_to_label_log(
            case_num,
            comment,
            success,
            attempt,
            current_temp,
            total_duration,
            model,
            pt,
            ct,
            topic_count
        )


def generate_topics(
    comments,
    question,
    start_batch=0,
    batch_size=80,
    batch_topics_max=8,
    max_themes_initial=30,
    temperature=0.2,
    topic_goal=18,
    temp_increase=0.1,
    max_retries=3
):
    """
    Generate and condense thematic topics from survey comments in batches.
    Splits `comments` into batches, extracts topics via LLM, then
    performs iterative generalization to reduce to `topic_goal`.

    Parameters:
        comments (list of str): Raw survey comments for topic extraction.
        question (str): Survey question for prompt context.
        start_batch (int): Zero-based batch index from which to resume.
        batch_size (int): Number of comments per batch.
        batch_topics_max (int): Max topics to extract per batch.
        max_themes_initial (int): Initial topic count threshold to trigger
                                   the first generalization step.
        temperature (float): Initial LLM sampling temperature.
        topic_goal (int): Target maximum number of final topics.
        temp_increase (float): Temperature increment for retries/generalization.
        max_retries (int): Number of LLM retry attempts per batch.

    Returns:
        list of dict: Final list of topic objects, each with 'title' and
                      'description', saved to disk as JSON.

    Notes:
        - Logs all interim topics and generalization outcomes to CSV.
        - Saves final topics to 'final_topics_{timestamp}.json' under SAVE_DIR.
        - Prints progress and token metrics to stdout.
    """
    json_schema_topics = {
        "name": "topic_description_schema",
        "schema": {
            "type": "object",
            "properties": {
                "topics": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "title": {"type": "string"},
                            "description": {"type": "string", "maxLength": 150}
                        },
                        "required": ["title", "description"],
                        "additionalProperties": False
                    }
                }
            },
            "required": ["topics"],
            "additionalProperties": False
        }
    }

    def append_process_log_csv(source, topics, success):
        """
        Log each batch or generalization result to the topic process log CSV.

        Parameters:
            source (str): Identifier for the step (e.g., 'Batch 1', 'Generalization').
            topics (list of dict): Generated topic dictionaries.
            success (bool): Whether the step succeeded in returning topics.

        Notes:
            - Writes header row if the CSV does not exist.
            - Logs each topic title and description on its own row.
        """
        file_exists = os.path.exists(topic_log_file)
        with open(topic_log_file, mode="a", newline="", encoding="utf-8") as f:
            writer = csv.writer(f)
            if not file_exists:
                writer.writerow(["Source", "Topic", "Description", "Success"])
            if topics:
                for t in topics:
                    writer.writerow([source, t.get("title", ""), t.get("description", ""), True])
            else:
                writer.writerow([source, "", "", False])

    def run_generalization(prompt_text, init_temp):
        """
        Iteratively condense a list of topics down to `topic_goal` via LLM.
        Increases temperature by `temp_increase` on each failure or if
        result exceeds `topic_goal`, up to a maximum of 1.0.

        Parameters:
            prompt_text (str): Prompt string for generalization step.
            init_temp (float): Starting temperature for the first attempt.

        Returns:
            list of dict or None: The condensed topics if successful;
                                  None if all attempts fail.

        Notes:
            - Logs each attempt's success or failure.
            - Prints timing and topic count metrics to stdout.
        """
        current_temp = init_temp
        while current_temp <= 1.0:
            try:
                start_time = time.time()
                response_text, _, pt, ct, model = query_llm(
                    prompt_text, temperature=current_temp, json_schema=json_schema_topics
                )
                elapsed = time.time() - start_time
                cleaned = extract_json_content(response_text)
                gen = json.loads(cleaned)
                new_topics = gen.get("topics", [])
                if not new_topics:
                    raise ValueError("No topics returned.")
                print(f"\n[Generalization] {len(new_topics)} topics in {elapsed:.0f}s ({ct} tokens, prompt {pt}).")
                if len(new_topics) <= topic_goal:
                    append_process_log_csv("Generalization", new_topics, True)
                    print(f"Generalization succeeded at temp {current_temp:.2f}.")
                    return new_topics
                print(f"{len(new_topics)} > goal {topic_goal}. Increasing temp...")
            except Exception as e:
                print(f"Error at temp {current_temp:.2f}: {e}. Increasing temp...")
            current_temp += temp_increase
            if current_temp <= 1.0:
                print(f"Increasing temperature to: {current_temp:.2f}")
        print("Generalization failed (temp > 1.0).")
        append_process_log_csv("Generalization", [], False)
        return None

    baseline_topics = None
    aggregated_topics = []
    num_batches = len(comments) // batch_size + (1 if len(comments) % batch_size else 0)

    for batch_num in range(start_batch, num_batches):
        pct = ((batch_num+1)/num_batches)*100
        print(f"\n[Batch {batch_num+1}/{num_batches} ({pct:.1f}%)]")
        batch_comments = comments[batch_num*batch_size:(batch_num+1)*batch_size]
        batch_prompt = TOPIC_BATCH_PROMPT.format(
            question=question,
            batch_topics_max=batch_topics_max,
            comments="\n".join(batch_comments)
        )

        batch_topics = []
        attempt = 0
        while attempt < max_retries:
            attempt += 1
            try:
                start = time.time()
                response_text, _, pt, ct, model = query_llm(
                    batch_prompt, temperature=temperature, json_schema=json_schema_topics
                )
                elapsed = time.time() - start
                cleaned = extract_json_content(response_text)
                batch_topics = json.loads(cleaned).get("topics", [])
                if not batch_topics:
                    raise ValueError("No topics returned.")
                print(f"[Batch {batch_num+1}] {len(batch_topics)} topics in {elapsed:.0f}s ({ct} tokens).")
                titles = [t.get("title", "") for t in batch_topics]
                print("Topics:", ", ".join(titles))
                break
            except Exception as e:
                print(f"Error batch {batch_num+1} attempt {attempt}: {e}")
                if attempt == max_retries:
                    print(f"Skipping batch {batch_num+1}.")
        append_process_log_csv(f"Batch {batch_num+1}", batch_topics, bool(batch_topics))

        if not batch_topics:
            continue

        if baseline_topics is None:
            aggregated_topics.extend(batch_topics)
            print(f"Accumulated {len(aggregated_topics)} topics (threshold {max_themes_initial}).")
            if len(aggregated_topics) >= max_themes_initial:
                prompt_text = TOPIC_GENERALIZATION_PROMPT.format(
                    question=question,
                    topic_goal=topic_goal,
                    aggregated=json.dumps(aggregated_topics, indent=2)
                )
                gen = run_generalization(prompt_text, temperature)
                baseline_topics = gen or aggregated_topics
                aggregated_topics = []
        else:
            prompt_text = TOPIC_GENERALIZATION_NEW_BATCH_PROMPT.format(
                question=question,
                topic_goal=topic_goal,
                baseline=json.dumps(baseline_topics, indent=2),
                new=json.dumps(batch_topics, indent=2)
            )
            gen = run_generalization(prompt_text, temperature)
            if gen:
                baseline_topics = gen
            else:
                print("Retaining previous baseline.")

    # Final consolidation if needed
    if baseline_topics is None:
        if len(aggregated_topics) > topic_goal:
            prompt_text = TOPIC_GENERALIZATION_PROMPT.format(
                question=question,
                topic_goal=topic_goal,
                aggregated=json.dumps(aggregated_topics, indent=2)
            )
            gen = run_generalization(prompt_text, temperature)
            baseline_topics = gen or aggregated_topics
        else:
            baseline_topics = aggregated_topics

    # Always log and save final topics
    append_process_log_csv("Final", baseline_topics, True)
    save_json(baseline_topics, f"final_topics_{timestamp}.json")
    return baseline_topics


# ================================
# USAGE
# ================================

# Specify the current survey question - this gets fed into the LLM prompts for context.
question = (
    "How could Conestoga improve student services, facilities and/or activities to better meet your needs?"
)

# Specify where your comments are - must be a CSV.
file_path = r"C:/Users/Fhay/Desktop/Local LLM/test.csv"
file_dir = os.path.dirname(file_path)

# Specify the column header of where the actual comments are in the CSV file above.
comments = load_comments(file_path, comment_column="Text")

# TOPIC SPECIFICATION
# Option 1: Start from scratch - no topics. The script will generate its own.
# Please check / edit the generate_topics() call below - see the function header comment for parameter details.
topics = load_topics()  # This will return None, and trigger topic generation.

# Option 2: Load existing topics from JSON that the script made previously. e.g.:
# topics = load_topics(r"C:\Users\Fhay\Desktop\Local LLM\topics.json")

# Option 3: Specify topic titles without any definitions:
# topics = ["Positive", "Neutral", "Negative"]

# Regular sequence check - if required, topics are generated.
# ------------------------------------------
if topics is None:
    topics = generate_topics(
        comments, question,
        start_batch=0,
        batch_size=67,
        batch_topics_max=15,
        max_themes_initial=28,
        temperature=0.29,
        topic_goal=12,
        temp_increase=0.18
    )
    print(f"Generated topics: {json.dumps(topics, indent=2)}")
# ------------------------------------------

# The main event - label comments using the (loaded or generated) topics. 
# Please check / edit the label_comments() call below - see the function header comment for parameter details.
label_comments(comments, question, topics, start_index=0, temperature=0.28, temp_increase=0.17)

print(f"\n✅ All steps completed. Labeled data appended to '{CSV_FILE}'")
