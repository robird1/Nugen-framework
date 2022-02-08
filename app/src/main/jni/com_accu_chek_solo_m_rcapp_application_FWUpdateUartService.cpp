#define LOG_NDEBUG 0 //This macro is used to switch show/disable logcat
#define LOG_TAG "JavaFrameworkService_jni"

#include "jni.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <android/log.h>


#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )


//This is java full name qualifier, use to assign java servcie framework
static const char* const kClassName = "com/accu_check/solo_m/rcapp/application/custframeworkservice/FWUpdateUartService";

/*
 * The method below are not thread-safe and not intended to be
 */
#ifdef CALLBACK
struct fields_t {
    jclass    clazzEffect;
    jmethodID midPostNativeEvent;
};
static fields_t fields;
#endif //CALLBACK

//*****************************************************************************
// FUNCTION NAME: foo
// DESCRIPTION:   This is dample
//
//
// ARGs:          JNIEnv: jni environment pointer
//                jclass: the object of the java servcie framework
//
// RETURNS:       int :
//*****************************************************************************
static jint
_testjni_foo(JNIEnv *env, jclass clazz)
{
    ALOGD("[%s] ++\n", __FUNCTION__);
    int ret = 0;

#ifdef CALLBACK
    eft_module_t const * module;

    fields.clazzEffect = NULL;

     // Get the AudioEffect class
    jclass clazz = env->FindClass(kClassName);
    if (clazz == NULL) {
        ALOGE("Can't find %s", kClassName);
        return;
    }

    fields.clazzEffect = (jclass)env->NewGlobalRef(clazz);


    // Get the postEvent method
    fields.midPostNativeEvent = env->GetStaticMethodID(
            fields.clazzEffect,
            "postEventFromNative", "(I)V");
    if (fields.midPostNativeEvent == NULL) {
        ALOGE("[%s]: Can't find AudioEffect.%s", "postEventFromNative", __FUNCTION__);
        return -1;
    }
#endif //CALLBACK

     ALOGD("[%s] --\n", __FUNCTION__);
    return ret;
}

//*****************************************************************************
// DESCRIPTION: jni functiion mapping table (JNINativeMethod)
//
//*****************************************************************************
static JNINativeMethod gMethods[] = {
    {"test1jnifoo",     "()I", (void*) _testjni_foo },
};


//*****************************************************************************
// FUNCTION NAME: registerMethods
// DESCRIPTION:   Register JNINativeMethod function table to DVM.
//
// ARGs:          JNIEnv: jni environment pointer
//
// RETURNS:       int :
//                  success ==> 0
//                  fail ==> -1
//*****************************************************************************
int register_custframeworkservice_fwupdateuartservice(JNIEnv* env)
{

    jclass clazz;

    /* look up the class */
    clazz = env->FindClass(kClassName);
    if (clazz == NULL) {
        ALOGE("Can't find class %s\n", kClassName);
        return -1;
    }

    /* register all the methods */
    if (env->RegisterNatives(clazz, gMethods,
            sizeof(gMethods) / sizeof(gMethods[0])) != JNI_OK)
    {
        ALOGE("Failed registering methods for %s\n", kClassName);
        return -1;
    }

    /* fill out the rest of the ID cache */
    return 0;
}




