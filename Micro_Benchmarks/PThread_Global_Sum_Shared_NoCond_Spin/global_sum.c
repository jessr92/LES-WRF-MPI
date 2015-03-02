#define _GNU_SOURCE
#include <stdio.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>

#define THREAD_COUNT 4
#define ITERATIONS 1000000

#ifdef __APPLE__
typedef int pthread_spinlock_t;
#endif

int thread_ids[THREAD_COUNT];
pthread_t threads[THREAD_COUNT];
pthread_spinlock_t spinlock;
int stillToWrite = 0;
int stillToRead = 0;
int opResult = 0;

#ifdef __APPLE__
int pthread_spin_init(pthread_spinlock_t *lock, int pshared) {
    __asm__ __volatile__ ("" ::: "memory");
    *lock = 0;
    return 0;
}

int pthread_spin_lock(pthread_spinlock_t *lock) {
    while (1) {
        int i;
        for (i=0; i < 10000; i++) {
            if (__sync_bool_compare_and_swap(lock, 0, 1)) {
                return 0;
            }
        }
        sched_yield();
    }
}

int pthread_spin_trylock(pthread_spinlock_t *lock) {
    if (__sync_bool_compare_and_swap(lock, 0, 1)) {
        return 0;
    }
    return -1;
}

int pthread_spin_unlock(pthread_spinlock_t *lock) {
    __asm__ __volatile__ ("" ::: "memory");
    *lock = 0;
    return 0;
}
#endif

int reduce(int);
int opAsMaster(int);
int opAsNonMaster(int);
int doOp(int id);
void *globalSum(void *);
int main(void);

int reduce(int value) {
    pthread_spin_lock(&spinlock);
    opResult += value;
    stillToWrite--;
    pthread_spin_unlock(&spinlock);
    while (stillToWrite != 0) {
        usleep(0);
    }
    return opResult;
}

int opAsMaster(int i) {
    while (stillToRead != 0) {
        usleep(0);
    }
    pthread_spin_lock(&spinlock);
    stillToWrite = THREAD_COUNT;
    stillToRead = THREAD_COUNT;
    opResult = 0;
    pthread_spin_unlock(&spinlock);
    return reduce(i);
}

int opAsNonMaster(int i) {
    while (stillToWrite == 0) {
        usleep(0);
    }
    return reduce(i);
}

int doOp(int id) {
    int result = (id == 0) ? opAsMaster(id) : opAsNonMaster(id);
    pthread_spin_lock(&spinlock);
    stillToRead--;
    pthread_spin_unlock(&spinlock);
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
    pthread_spin_init(&spinlock, PTHREAD_PROCESS_SHARED);
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
