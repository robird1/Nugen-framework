//********** COPYRIGHT 2013 altek Corporation *********************************
// FILE NAME:   This is template file.
// VERSION:     $Revision: 19842 $
// DESCRIPTION: The file is for java framework service jni
// 
//
//*****************************************************************************
// UPDATE LOG:
// $Log: $
//*****************************************************************************
//****************** STANDARD CPP-INCLUDE FILES *********************************
//************* GLOBAL AND PROJECT INCLUDE FILES ******************************
#define LOG_NDEBUG 0 //This macro is used to switch show/disable logcat
#define LOG_TAG "CommsJNI"

#include <dlfcn.h>
#include <jni.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <android/log.h>

#include "custHal.h"

//#include <cmd_msg_err.h>
//#include <cmd_msg_wrapper.h>
//#include <cmd_msg_parser.h>


#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )


//This is java full name qualifier, use to assign java servcie framework
static const char* const kClassName = "com/accu_chek/solo_m/rcapp/communication/uicommanddispatcher/CommsJNI";

//cust hal instance
static CUSTHAL *getHal = NULL;
     
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


/*-------------------------------------------------------------------
* JNI functions
* -----------------------------------------------------------------*/
static jint _power_on(JNIEnv *env, jclass clazz);
static jint _power_off(JNIEnv *env, jclass clazz);


//*****************************************************************************
// DESCRIPTION: jni functiion mapping table (JNINativeMethod)
//
//*****************************************************************************
static JNINativeMethod gMethods[] = {
    {"_powerOn",     "()I", (void*) _power_on },
    {"_powerOff",     "()I", (void*) _power_off }
};


//*****************************************************************************
// FUNCTION NAME: registerMethods
// DESCRIPTION:   Register JNINativeMethod function table to DVM.
//
// ARGs:          JNIEnv: jni environment
//
//
// RETURNS:       int : 
//                  success ==> 0
//                  fail ==> -1
//***************************************************************************** 
int register_custframeworkservice_commsjni(JNIEnv* env)
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

//    getHal = CUSTHAL::getInstance();
//
//    //Open gpio, adc driver path
//    getHal->initCustDevice();

    /* fill out the rest of the ID cache */
    return 0;
}



//*****************************************************************************
// FUNCTION NAME: _power_on
// DESCRIPTION: power on CommsSubsystem
//
//
// ARGs:          JNIEnv: jni environment pointer
//                jclass: the object of the java servcie framework
//
// RETURNS:       int :
//*****************************************************************************

static jint
_power_on(JNIEnv *env, jclass clazz)
{
    ALOGD("[%s] ++\n", __FUNCTION__);
    int ret = -1;   //error
    ret = CUSTHAL::getInstance()->openCommsDevice();
    ALOGV("[%s]: power on function ret[%d]\n", __FUNCTION__, ret);

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
// FUNCTION NAME: _power_off
// DESCRIPTION: power off CommsSubsystem
//
//
// ARGs:          JNIEnv: jni environment pointer
//                jclass: the object of the java servcie framework
//
// RETURNS:       int :
//*****************************************************************************

static jint
_power_off(JNIEnv *env, jclass clazz)
{
    ALOGD("[%s] ++\n", __FUNCTION__);
    int ret = -1;   //error
    ret = CUSTHAL::getInstance()->closeCommsDevice();
    ALOGV("[%s]: power on function ret[%d]\n", __FUNCTION__, ret);

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


