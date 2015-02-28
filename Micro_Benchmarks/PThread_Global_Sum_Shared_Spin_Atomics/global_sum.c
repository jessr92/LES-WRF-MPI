#define _GNU_SOURCE
#include <stdio.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdatomic.h>

#define THREAD_COUNT 4
#define ITERATIONS 100000
#define TEST

// Doesn't currently work

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
    atomic_fetch_add_explicit(&opResult, value, memory_order_relaxed);
    atomic_fetch_sub_explicit(&stillToWrite, 1, memory_order_relaxed);
    while (atomic_load_explicit(&stillToWrite, memory_order_relaxed) != 0) {
        usleep(0);
    }
    return atomic_load_explicit(&opResult, memory_order_relaxed);
}

int opAsMaster(int i) {
    while (atomic_load_explicit(&stillToRead, memory_order_relaxed) != 0) {
        usleep(0);
    }
    atomic_store_explicit(&stillToWrite, THREAD_COUNT, memory_order_relaxed);
    atomic_store_explicit(&stillToRead , THREAD_COUNT, memory_order_relaxed);
    atomic_store_explicit(&opResult, 0, memory_order_relaxed);
    return reduce(i);
}

int opAsNonMaster(int i) {
    while (atomic_load_explicit(&stillToWrite, memory_order_relaxed) == 0) {
        usleep(0);
    }
    return reduce(i);
}

int doOp(int id) {
    int result = (id == 0) ? opAsMaster(id) : opAsNonMaster(id);
    atomic_fetch_sub_explicit(&stillToRead, 1, memory_order_relaxed);
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
#ifdef TEST
        printf("Thread %d finished with result %d\n", id, result);
#endif
    }
    printf("Thread %d finished with result %d\n", id, result);
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
