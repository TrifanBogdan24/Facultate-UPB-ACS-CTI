#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct proc{
        short pid;
        char prio;
        short time;
};

void sort_procs(struct proc *procs, int len)
{
    for (int i = 0; i <= len - 2; i++) {
        for (int j = i + 1; j <= len - 1; j++) {
            int to_be_swaped = 0;
            
            if (procs[i].prio > procs[j].prio)
                to_be_swaped = 1;
            
            if (procs[i].prio == procs[j].prio && procs[i].time > procs[j].time) 
                to_be_swaped = 1;

            if (procs[i].prio == procs[j].prio && procs[i].time == procs[j].time && procs[i].pid > procs[j].pid)
                to_be_swaped = 1;
            
            if (to_be_swaped) {
                struct proc aux = procs[i];
                procs[i] = procs[j];
                procs[j] = aux;
            }
        }
    }
}

int main()
{
    struct proc *procs = (struct proc *) malloc(100 * sizeof(struct proc)); // pid, prio, time
    int n = 8;
    procs[0].pid = 101; procs[0].prio = '4'; procs[0].time = 50;
    procs[1].pid = 93; procs[1].prio = '4'; procs[0].time = 10;
    procs[2].pid = 189; procs[2].prio = '4'; procs[2].time = 5;
    procs[3].pid = 161; procs[3].prio = '1'; procs[0].time = 12;
    procs[4].pid = 671; procs[4].prio = '1'; procs[4].time = 121;
    procs[5].pid = 901; procs[5].prio = '1'; procs[5].time = 5;
    procs[6].pid = 211; procs[6].prio = '3'; procs[0].time = 11;
    procs[7].pid = 21; procs[7].prio = '2'; procs[7].time = 1;
    sort_procs(procs, 8); // prio -> time -> pid
    printf("prio   time   pid\n");
    for (int i = 0; i < n; i++) {
        printf(" %c      %hi    %hi\n", procs[i].prio, procs[i].time, procs[i].pid);
    }
    return 0;
}
