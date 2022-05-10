#!/usr/bin/python3
#%%
m = 50

print('let g(x,y) = x .+. y')
print('let f0(x) = x')
print('let f1(x) = f0(x)')
for i in range(0,m):
    print(f'let f{i+2}(x) = g(f{i}(x),f{i+1}(x))')

print(f'print "test" f{m+1}(0)')
# %%
