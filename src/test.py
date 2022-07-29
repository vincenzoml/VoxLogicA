import random

j = 10

for i in range(0,1000):
    k = j + random.randint(0,5)

    print(f'''print "fibBackground({k},{i})" fibBackground({k},{i})''')