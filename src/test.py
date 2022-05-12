#!/usr/bin/python3
#%%

m = 100000

doc = 'let t0(x) = x'

for x in range(0,m):
    doc = doc + f"\nlet t{x+1}(x) = t{x}(x)"

doc = doc + f'\nprint "t" t{m}(0)'

print(doc)


# %%
