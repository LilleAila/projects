import pandas as pd

data = pd.read_csv("ecommerce_sales.csv")
categories = data["ProductCategory"].value_counts()

print(categories)
