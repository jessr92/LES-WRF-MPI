#define _GNU_SOURCE
#include <stdio.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>

#define THREAD_COUNT 4
#define ITERATIONS 10000

int thread_ids[THREAD_COUNT];
pthread_t threads[THREAD_COUNT];
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
int stillToWrite = 0;
int stillToRead = 0;
int opResult = 0;

int reduce(int value) {
    pthread_mutex_lock(&mutex);
    opResult += value;
    stillToWrite--;
    if (stillToWrite == 0) {
        pthread_cond_broadcast(&cond);
    } else {
        while (stillToWrite != 0) {
            pthread_cond_wait(&cond, &mutex);
        }
    }
    pthread_mutex_unlock(&mutex);
    return opResult;
}

int opAsMaster(int i) {
    pthread_mutex_lock(&mutex);
    while (stillToRead != 0) {
        pthread_cond_wait(&cond, &mutex);
    }
    stillToWrite = THREAD_COUNT;
    stillToRead = THREAD_COUNT;
    opResult = 0;
    pthread_mutex_unlock(&mutex);
    pthread_cond_broadcast(&cond);
    return reduce(i);
}

int opAsNonMaster(int i) {
    pthread_mutex_lock(&mutex);
    while (stillToWrite == 0) {
        pthread_cond_wait(&cond, &mutex);
    }
    pthread_mutex_unlock(&mutex);
    return reduce(i);
}

int doOp(int id) {
    int result = (id == 0) ? opAsMaster(id) : opAsNonMaster(id);
    pthread_mutex_lock(&mutex);
    stillToRead--;
    pthread_mutex_unlock(&mutex);
    if (stillToRead == 0) {
        pthread_cond_broadcast(&cond);
    }
    return result;
}

void *globalSum(void* thread_id) {
    int id = *(int*)thread_id;
    int result = 0;
    printf("ID: %lu, CPU: %d\n", pthread_self(), sched_getcpu());
    for (int i=0; i < ITERATIONS; i++) {
        result = doOp(id);
    }
    printf("Thread %d finished with result %d\n", id, result);
}

void main() {
    cpu_set_t cpuset;
    pthread_t thread;
    pthread_attr_t attr;
    struct timeval start, end;
    pthread_attr_init(&attr);
    for (int i=0; i < THREAD_COUNT; i++) {
        CPU_ZERO(&cpuset);
        CPU_SET(i, &cpuset);
        thread_ids[i] = i;
        pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cpuset);
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
}

