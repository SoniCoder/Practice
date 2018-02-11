#https://www.geeksforgeeks.org/detect-cycle-in-a-graph/

'''Please note that it's Function problem i.e.
you need to write your solution in the form of Function(s) only.
Driver Code to call/invoke your function would be added by GfG's Online Judge.'''


# Your task is to complete this function
# Function should return True/False or 1/0
# Graph(graph) is a defaultict of type List
# n is no of Vertices

class Node:
    def __init__(self):
        self.children = set()


def isCyclic(n, graph):
    Nodes = [Node() for n_i in range(n)]
    for k in graph:
        Nodes[k].children = set(graph[k])
    grey = set()
    visited = set()
    stack = []
    #print(graph)
    for node_i in range(n):
        if node_i not in visited:
            stack.append(node_i)
        while stack:
            item = stack.pop()
            if type(item) != int:
                item()
                continue
            if item in visited: continue
            visited.add(item)
            grey.add(item)
            stack.append(lambda item=item: grey.remove(item))
            #print(stack)
            for ch in Nodes[item].children:
                if ch in grey:
                    return True
                if ch not in visited:
                    stack.append(ch)
    return False
