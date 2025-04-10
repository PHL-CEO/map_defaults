import pandas as pd
from passyunk.parser import PassyunkParser
p = PassyunkParser()

df = pd.read_csv('INSERT CSV FILE PATH')
df.columns = df.columns.str.strip()


def standardize_address(address):
    try:
        parsed = p.parse(address)
        return parsed['components']['base_address']
    except Exception:
        return "error"


df['standardized_address'] = df["INSERT COLUMN NAME FOR ADDRESS"].apply(standardize_address)

df.to_csv('OUT FILE PATH_parsed.csv', index=False)