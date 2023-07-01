from graphs import adjacency_graph0 as graph


def greedy_color(graph):
    vertex_colors = [-1] * len(graph)
    for i in range(len(vertex_colors)):
        if vertex_colors[i] == -1:
            for v in range(len(graph)):
                if vertex_colors[v] == -1:
                    can_color = True
                    for n in graph[v]:
                        if vertex_colors[n] == i:
                            can_color = False
                            break
                    if can_color:
                        vertex_colors[v] = i
    return vertex_colors


if __name__ == '__main__':
    print(greedy_color(graph))
