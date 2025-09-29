import pandas as pd
import matplotlib.pyplot as plt

# Load dataset
df = pd.read_csv('./ecommerce_sales.csv')
print(df.head())

print("Total Orders:", df['OrderID'].nunique())

# Total revenue
total_revenue = df['Total'].sum()
# Top 5 products by revenue
top_products = df.groupby('Product')['Total'].sum().sort_values(ascending=False).head(5)

# Most popular product category
print("??:", df['ProductCategory'].value_counts().idxmax())

category_revenue = df.groupby('ProductCategory')['Total'].sum()
print("??:", category_revenue.idxmax(), ":", category_revenue.max())


# Orders by payment method
print(df['PaymentMethod'].value_counts())

# City with highest revenue
city_revenue = df.groupby('City')['Total'].sum()

# Check for missing values
print(df.isnull().sum())

# Drop duplicates
df = df.drop_duplicates()

# Average order value
aov = df['Total'].mean()

# Ensure OrderDate is in datetime format
df['OrderDate'] = pd.to_datetime(df['OrderDate'])

# Extract the month
df['Month'] = df['OrderDate'].dt.month

# Group by Month and sum Total revenue
monthly_revenue = df.groupby('Month')['Total'].sum()

print(monthly_revenue)

product_revenue = df.groupby('Product')['Total'].sum().sort_values(ascending=False)

# Monthly revenue bar chart
monthly_revenue.plot(kind='bar', title='Monthly Revenue')
plt.xlabel('Month')
plt.ylabel('Revenue')
plt.show()

# Payment method pie chart
df['PaymentMethod'].value_counts().plot(kind='pie', autopct='%1.1f%%', title='Payment Methods')
plt.show()
