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
#define LOG_TAG "CustRunTime"


#include "jni.h"

#include <custHal.h>

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


#ifndef NELEM
# define NELEM(x) ((int) (sizeof(x) / sizeof((x)[0])))
#endif

/*
 * JNI-based registration functions.  Note these are properly contained in
 * namespace android.
 */
extern int register_custframeworkservice_fwupdateuartservice(JNIEnv* env);
extern int register_custframeworkservice_commsjni(JNIEnv* env);
extern int register_custinterface_customizedhwmanager_uart(JNIEnv* env);
extern int register_custinterface_customizedhwmanager_uicp(JNIEnv* env);
extern int register_custframeworkservice_BgmHWControlService(JNIEnv* env);
extern int register_custinterface_customizedhwmanager_led(JNIEnv* env);
extern int register_custinterface_customizedhwmanager_haptic(JNIEnv* env);
extern int register_custinterface_customizedhwmanager_adc(JNIEnv* env);
extern int register_custframeworkservice_NugenUEventObserver(JNIEnv* env);
extern int register_custframeworkservice_KeyExchange(JNIEnv* env);

#ifdef LOG_NDEBUG
    #define REG_JNI(name)      { name }
    struct RegJNIRec {
        int (*mProc)(JNIEnv*);
    };
#else
    #define REG_JNI(name)      { name, #name }
    struct RegJNIRec {
        int (*mProc)(JNIEnv*);
        const char* mName;
    };
#endif



static int register_jni_procs(const RegJNIRec array[], size_t count, JNIEnv* env)
{
    for (size_t i = 0; i < count; i++) {
        if (array[i].mProc(env) < 0) {
            ALOGD("----------!!! failed to load\n");
            return -1;
        }
    }
    return 0;
}

static const RegJNIRec gRegJNI[] = {
		//REG_JNI(register_custframeworkservice_fwupdateuartservice),
		REG_JNI(register_custframeworkservice_commsjni),
		REG_JNI(register_custinterface_customizedhwmanager_uart),
		REG_JNI(register_custinterface_customizedhwmanager_uicp),
		REG_JNI(register_custframeworkservice_BgmHWControlService),
		REG_JNI(register_custinterface_customizedhwmanager_led),
		REG_JNI(register_custinterface_customizedhwmanager_haptic),
		REG_JNI(register_custinterface_customizedhwmanager_adc),
		REG_JNI(register_custframeworkservice_NugenUEventObserver),
		REG_JNI(register_custframeworkservice_KeyExchange),
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
static int registerMethods(JNIEnv* env) 
{

	    ALOGV("--- registering native functions ---\n");

	    /*
	     * Every "register" function calls one or more things that return
	     * a local reference (e.g. FindClass).  Because we haven't really
	     * started the VM yet, they're all getting stored in the base frame
	     * and never released.  Use Push/Pop to manage the storage.
	     */
	    env->PushLocalFrame(200);

	    if (register_jni_procs(gRegJNI, NELEM(gRegJNI), env) < 0) {
	        ALOGE("register_jni_procs function fail...\n");
	        env->PopLocalFrame(NULL);
	        return -1;
	    }
	    env->PopLocalFrame(NULL);


    /* fill out the rest of the ID cache */
    return 0;
}


// ----------------------------------------------------------------------------
//*****************************************************************************
// FUNCTION NAME: JNI_OnLoad
// DESCRIPTION:   This is called by the VM when the shared library is first loaded.
//
// ARGs:          JNIEnv: jni environment pointer
//
// RETURNS:       int : 
//                  success ==> JNI_VERSION_1_6
//                  fail ==> -1
//***************************************************************************** 
jint JNI_OnLoad(JavaVM* vm, void* reserved) 
{
    JNIEnv* env = NULL;
    jint result = -1;

    if (vm->GetEnv((void**) &env, JNI_VERSION_1_4) != JNI_OK) {
        ALOGE("ERROR: GetEnv failed\n");
        goto bail;
    }
    assert(env != NULL);

    //Open gpio, adc driver path
    CUSTHAL::getInstance()->initCustDevice();

    if (registerMethods(env) != 0) {
        ALOGE("ERROR: PlatformLibrary native registration failed\n");
        goto bail;
    }

    /* success -- return valid version number */
    result = JNI_VERSION_1_6;

bail:
    return result;
}


