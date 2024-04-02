#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define MAX_COORD 100
#define MAX_NODES 10

typedef struct {
    char *id;
    int x;
    int y;
} Node;

void generate_nodes(Node nodes[], int num_nodes) {
    for (int i = 0; i < num_nodes; i++) {
        nodes[i].id = malloc(10 * sizeof(char));
        sprintf(nodes[i].id, "Node%d", i);
        nodes[i].x = rand() % MAX_COORD;
        nodes[i].y = rand() % MAX_COORD;
    }
}

void generate_links(Node nodes[], int num_nodes) {
    for (int i = 0; i < num_nodes; i++) {
        for (int j = i + 1; j < num_nodes; j++) {
            if (rand() % 2) {
                printf("- %s -> %s\n", nodes[i].id, nodes[j].id);
            }
        }
    }
}

int main(int argc, char **argv) {

    if (argc != 2) {
        printf("Usage : ./creer_graphe <nb_noeuds>\n");
        return 1;
    }

    srand(time(NULL));

    int num_nodes = atoi(argv[1]);
    Node nodes[num_nodes];

    printf("Noeuds:\n");
    generate_nodes(nodes, num_nodes);
    for (int i = 0; i < num_nodes; i++) {
        printf("- %s: (%d, %d)\n", nodes[i].id, nodes[i].x, nodes[i].y);
    }

    printf("\nLiens:\n");
    generate_links(nodes, num_nodes);

    for (int i = 0; i < num_nodes; i++) {
        free(nodes[i].id);
    }

    return 0;
}