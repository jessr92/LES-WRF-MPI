#include <stdio.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>

#define THREAD_COUNT 4
#define ITERATIONS 10000

int thread_ids[THREAD_COUNT];
pthread_t threads[THREAD_COUNT];
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t condA = PTHREAD_COND_INITIALIZER;
pthread_cond_t condB = PTHREAD_COND_INITIALIZER;
pthread_cond_t condC = PTHREAD_COND_INITIALIZER;
int stillToWrite = 0;
int stillToRead = 0;
int opResult = 0;

int reduce(int value) {
    pthread_mutex_lock(&mutex);
    opResult += value;
    stillToWrite--;
    if (stillToWrite == 0) {
        pthread_cond_broadcast(&condB);
    } else {
        while (stillToWrite != 0) {
            pthread_cond_wait(&condB, &mutex);
        }
    }
    pthread_mutex_unlock(&mutex);
    return opResult;
}

int opAsMaster(int i) {
    pthread_mutex_lock(&mutex);
    while (stillToRead != 0) {
        pthread_cond_wait(&condC, &mutex);
    }
    stillToWrite = THREAD_COUNT;
    stillToRead = THREAD_COUNT;
    opResult = 0;
    pthread_mutex_unlock(&mutex);
    pthread_cond_broadcast(&condA);
    return reduce(i);
}

int opAsNonMaster(int i) {
    pthread_mutex_lock(&mutex);
    while (stillToWrite == 0) {
        pthread_cond_wait(&condA, &mutex);
    }
    pthread_mutex_unlock(&mutex);
    return reduce(i);
}

void *globalSum(void* thread_id) {
    int id = *(int*)thread_id;
    int result = 0;
    for (int i=0; i < ITERATIONS; i++) {
        result = (id == 0) ? opAsMaster(id) : opAsNonMaster(id);
        pthread_mutex_lock(&mutex);
        stillToRead--;
        pthread_mutex_unlock(&mutex);
        if (stillToRead == 0) {
            pthread_cond_broadcast(&condC);
        }
    }
    printf("Thread %d finished with result %d\n", id, result);
}

void main() {
    pthread_t thread;
    int returnValue;
    struct timeval start, end;
    for (int i=1; i < THREAD_COUNT; i++) {
        thread_ids[i] = i;
        pthread_create(&threads[i], NULL, globalSum, (void *) &thread_ids[i]);
    }
    gettimeofday(&start, 0);
    thread_ids[0] = 0;
    globalSum((void *) &thread_ids[0]);
    for (int i=1; i < THREAD_COUNT; i++) {
        pthread_join(threads[i], NULL);
    }
    gettimeofday(&end, 0);
    long long elapsed_time = ((end.tv_sec - start.tv_sec) * 1000000LL) + end.tv_usec - start.tv_usec;
    double seconds = elapsed_time / 1000000.0;
    printf("Wall clock time: %f, Iterations per second: %f\n", seconds, ITERATIONS/seconds);
}
