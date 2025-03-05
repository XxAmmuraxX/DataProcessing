import pandas as pd
import re
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.stem import WordNetLemmatizer

nltk.download('punkt')
nltk.download('wordnet')
nltk.download('stopwords')

lemmatizer = WordNetLemmatizer()
stop_words = set(stopwords.words('english'))

def clean_text_pipeline(series):

    def clean_text(text):
        def mapping(x):
            """ mapping words tags into characters recignized by lemmatizer """
            if x[1] == 'ADV':
                pos = 'r'
            elif x[1] == 'ADJ':
                pos = 'a'
            elif x[1] == 'VERB':
                pos = 'v'
            else:
                pos = 'n'
            return (x[0], pos)

        # Step 1: Convert text to lowercase
        text = text.lower()

        # Step 2: Remove punctuation and numbers
        text = re.sub(r'<.?>', ' ', text)
        text = re.sub(r'\d+', ' ', text)
        text = re.sub(r'\W+', ' ', text)

        # Step 3: Tokenize text
        tokens = word_tokenize(text)

        # Step 4: Remove stop words and lemmatize
        tags = nltk.pos_tag(tokens, tagset='universal')

        correted_tag = list(map(mapping, tags))

        cleaned_tokens = [lemmatizer.lemmatize(word, pos=pos) \
                          for word, pos in correted_tag if word not in stop_words]

        # Step 5: Join tokens back into a single string
        return ' '.join(cleaned_tokens)

    # Apply the cleaning function to the Series
    return series.apply(clean_text)