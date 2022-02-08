/*
 * Copyright (C) 2008 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#define LOG_TAG "NugenUEventObserver_jni"
//#define LOG_NDEBUG 0

#include <cstddef>
#include <dlfcn.h>

#include "jni.h"

#include <pthread.h>

#include <android/log.h>

using namespace std;

#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )


void *mHandle;

pthread_mutex_t mutex;

static const char* const kClassName = "com/accu_chek/solo_m/rcapp/application/CustUEventObserver";

static void OpenHardWareLib()
{
	const char* path = "/system/lib/libhardware_legacy.so";
	mHandle = dlopen(path, RTLD_NOW);

	if (mHandle == NULL) {
		char const *err_str = dlerror();
		ALOGE("load: module=%s\n%s", path, err_str?err_str:"unknown");
	}



	//reset
	dlerror();
	ALOGV("[%s]: exit\n",__FUNCTION__);
}

static int ueventInit()
{
	ALOGV("[%s]: enter\n",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "uevent_init";
	int (*pInit)() = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	pInit = (int(*)())dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*pInit)();
		ALOGV("[%s]: function call sucessful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit\n",__FUNCTION__);
	return ret;
}

static int UeventNextEvent(char* buffer, int buffer_length)
{
	ALOGV("[%s]: enter\n",__FUNCTION__);
		int ret = -1;
		const char *error = NULL;
		const char* funcName = "uevent_next_event";
		int (*pNext)(char*, int) = NULL;

		if(mHandle == NULL) {
			ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
			goto DOWN;
		}

		pNext = (int(*)(char*, int))dlsym(mHandle, funcName);

		if ((error = dlerror()) != NULL)  {
			ALOGE("[%s]: %s\n", __FUNCTION__, error);
		}
		else {
			ret = (*pNext)(buffer, buffer_length);
			ALOGV("[%s]: function call sucessful\n",__FUNCTION__);
		}

DOWN:
		ALOGV("[%s]: exit\n",__FUNCTION__);
		return ret;
}

static void nativeSetup(JNIEnv *env, jclass clazz) {
    if (!ueventInit()) {
    	jclass Exception = env->FindClass("java/lang/Exception");
    	        env->ThrowNew(Exception,"Unable to open socket for UEventObserver");
    }
}

static jstring nativeWaitForNextEvent(JNIEnv *env, jclass clazz) {
    char buffer[1024];

    for (;;) {
        int length = UeventNextEvent(buffer, sizeof(buffer) - 1);
        if (length <= 0) {
            return NULL;
        }
        buffer[length] = '\0';

        ALOGV("Received uevent message: %s", buffer);

        jchar message[length];
        for (int i = 0; i < length; i++) {
        	message[i] = buffer[i];
        }
        return env->NewString(message, length);
    }
}

static JNINativeMethod gMethods[] = {
    { "nativeSetup", "()V",
            (void *)nativeSetup },
    { "nativeWaitForNextEvent", "()Ljava/lang/String;",
            (void *)nativeWaitForNextEvent },
};


int register_custframeworkservice_NugenUEventObserver(JNIEnv *env)
{
    jclass clazz;

    clazz = env->FindClass(kClassName);
    if (clazz == NULL) {
        ALOGE("Can't find NugenUEventObserver");
        return -1;
    }

    /* register all the methods */
    if (env->RegisterNatives(clazz, gMethods,
                sizeof(gMethods) / sizeof(gMethods[0])) != JNI_OK)
    {
            ALOGE("Failed registering methods for %s\n", kClassName);
            return -1;
    }

    OpenHardWareLib();

    pthread_mutex_init (&mutex, NULL);

    /* fill out the rest of the ID cache */
    return 0;

}
