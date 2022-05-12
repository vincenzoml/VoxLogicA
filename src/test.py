#!/usr/bin/python3
#%%

m = 10000

doc = f'''
let f(x,y) = x .*. y
let g(x) = x .*. 2
let h(x) = x .*. 7
let F(x) = f(g(h(x)),h(g(x)))
let t(x) = F x
'''

for x in range(1,m):
    doc = doc + "\nlet t(x) = F(t(x))"

doc = doc + '\nprint "t" t(0)'

print(doc)


# %%
