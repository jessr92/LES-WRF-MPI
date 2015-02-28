#define _GNU_SOURCE
#include <stdio.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdatomic.h>

#define THREAD_COUNT 8
#define ITERATIONS 100000

int thread_ids[THREAD_COUNT];
pthread_t threads[THREAD_COUNT];
_Atomic int stillToWrite = ATOMIC_VAR_INIT(0);
_Atomic int stillToRead = ATOMIC_VAR_INIT(0);
_Atomic int opResult = ATOMIC_VAR_INIT(0);

int reduce(int);
int opAsMaster(int);
int opAsNonMaster(int);
int doOp(int id);
void *globalSum(void *);
int main(void);

int reduce(int value) {
    atomic_fetch_add(&opResult, value);
    atomic_fetch_sub(&stillToWrite, 1);
    while (atomic_load(&stillToWrite) != 0) {
        usleep(0);
    }
    return atomic_load(&opResult);
}

int opAsMaster(int i) {
    while (atomic_load(&stillToRead) != 0) {
        usleep(0);
    }
    atomic_store(&stillToWrite, THREAD_COUNT);
    atomic_store(&stillToRead , THREAD_COUNT);
    atomic_store(&opResult, 0);
    return reduce(i);
}

int opAsNonMaster(int i) {
    while (atomic_load(&stillToWrite) == 0) {
        usleep(0);
    }
    return reduce(i);
}

int doOp(int id) {
    int result = (id == 0) ? opAsMaster(id) : opAsNonMaster(id);
    atomic_fetch_sub(&stillToRead, 1);
    return result;
}

void *globalSum(void* thread_id) {
    int id = *(int*)thread_id;
    int result = 0;
#ifndef __APPLE__
    printf("ID: %lu, CPU: %d\n", pthread_self(), sched_getcpu());
#endif
    for (int i=0; i < ITERATIONS; i++) {
        result = doOp(id);
        printf("Thread %d finished with result %d\n", id, result);
    }
    return NULL;
}

int main(void) {
#ifndef __APPLE__
    cpu_set_t cpuset;
#endif
    pthread_attr_t attr;
    struct timeval start, end;
    pthread_attr_init(&attr);
    for (int i=0; i < THREAD_COUNT; i++) {
#ifndef __APPLE__
        CPU_ZERO(&cpuset);
        CPU_SET(i, &cpuset);
        pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cpuset);
#endif
        thread_ids[i] = i;
        pthread_create(&threads[i], &attr, globalSum, (void *) &thread_ids[i]);

    }
    gettimeofday(&start, 0);
    for (int i=1; i < THREAD_COUNT; i++) {
        pthread_join(threads[i], NULL);
    }
    gettimeofday(&end, 0);
    long long elapsed_time = ((end.tv_sec - start.tv_sec) * 1000000LL) + end.tv_usec - start.tv_usec;
    double seconds = elapsed_time / 1000000.0;
    printf("Wall clock time: %f, Iterations per second: %f\n", seconds, ITERATIONS/seconds);
    return 0;
}
