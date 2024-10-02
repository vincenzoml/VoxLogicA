#!/usr/bin/python3
#%%

import sys


m = 5000

if len(sys.argv) > 1:
    m = int(sys.argv[1])

doc = f'''
let f(x,y) = x .*. y
let g(x) = x .*. 2
let h(x) = x .*. 7
let F(x) = f(g(h(f(x,g(x)))),h(g(g(x))))
let t0(x) = F x
'''

for x in range(0,m):
    doc = doc + f"\nlet t{x+1}(x) = F(t{x}(x))"

doc = doc + f'\nprint "t" t{m}(0)'

print(doc)


# %%
