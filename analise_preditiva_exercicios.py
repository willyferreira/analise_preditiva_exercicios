import pandas as pd
%pip install openpyxl

df = pd.read_excel(
    io = 'dataset.xlsx')

df.head()