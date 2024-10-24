
"""
This code is from a sample from my current research assistantship. I want to analyze the FOMC meeting minutes sentence by sentence.
The given data is formatted with an entire section in one row of the dataset, with another column for the section header and meeting date. This script takes 
that dataset as input and returns an excel file with a sheet for each meeting date. In each sheet the sentences are separated into rows with a column for the section header and word count of the sentence.
"""

import pandas as pd

#Function to sanitize the meeting date for the Excel sheet name
def clean_sheet_name(meeting_date):
    # Convert the date to just the date part (YYYY-MM-DD)
    clean_date = pd.to_datetime(meeting_date).strftime('%Y-%m-%d')
    return clean_date

# Define the function to process FOMC data.
def process_fomc_data(df):
    # Create a dictionary to store the data by meeting date
    meetings_data = {}

    # Process each row to create a sheet per meeting date
    for idx, row in df.iterrows():
        meeting_date = row["Meeting Date"]
        section = row["Section"]
        text = row["Text"]
        
        # Split the text into sentences
        sentences = text.split('. ')
        
        # Remove empty strings and strip
        sentences = [sentence.strip() for sentence in sentences if sentence]  

        # Initialize a DataFrame for this meeting if not already done
        if meeting_date not in meetings_data:
            meetings_data[meeting_date] = []

        # Append sentences and their corresponding section to the meeting data
        for sentence in sentences:
            meetings_data[meeting_date].append({
                "Sentence": sentence,
                "Section": section,
                "Word Count": len(sentence.split())
            })

    # Create a new Excel writer object to save the output
    output_file = "fomc_meetings_output.xlsx"  
    
    with pd.ExcelWriter(output_file, engine="xlsxwriter") as writer:
        
        # Write each meeting's data to a separate sheet
        for meeting_date, meeting_data in meetings_data.items():
            sanitized_sheet_name = clean_sheet_name(meeting_date)
            meeting_df = pd.DataFrame(meeting_data)
            
            # Write to Excel sheet named after the sanitized meeting date
            meeting_df.to_excel(writer, sheet_name=sanitized_sheet_name, index=False)

    print("Spreadsheet with processed FOMC data has been created.")


if __name__ == '__main__':
    
    input_file = "FOMC minutes all_sentences_by_section_since_2010.xlsx"  
    
    # Fill any potential missing values with empty strings
    df = pd.read_excel(input_file)
    df["Meeting Date"] = df["Meeting Date"].fillna("").astype(str)
    df["Section"] = df["Section"].fillna("").astype(str)
    df["Text"] = df["Text"].fillna("").astype(str)
    print(df.info())
    
    
    # Process the file
    process_fomc_data(df)
