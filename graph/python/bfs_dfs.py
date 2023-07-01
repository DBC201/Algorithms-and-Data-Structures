from graphs import adjacency_graph0 as graph


def bfs(graph):
    visited = [False] * len(graph)
    queue = []
    queue.append(0)
    while len(queue) > 0:
        vertex = queue.pop(0)
        visited[vertex] = True
        print(vertex)
        for neighbor in graph[vertex]:
            if not visited[neighbor]:
                queue.append(neighbor)
                visited[neighbor] = True


def dfs(graph):
    visited = [False] * len(graph)

    def recurse(vertex):
        print(vertex)
        visited[vertex] = True
        for neighbor in graph[vertex]:
            if not visited[neighbor]:
                recurse(neighbor)

    recurse(0)


if __name__ == '__main__':
    bfs(graph)
    print("-------------")
    dfs(graph)
