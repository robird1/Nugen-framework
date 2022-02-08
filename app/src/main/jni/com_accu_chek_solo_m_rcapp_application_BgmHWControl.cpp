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
#define LOG_TAG "BGMHWControl"

#include <jni.h>
#include <custHal.h>
#include <NativeUART.h>
#include <NativeUICP.h>

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
static const char* const kClassName = "com/accu_chek/solo_m/rcapp/application/bgmcontrol/BgmJni";

static jboolean Bgm_power_on(JNIEnv *env, jclass clazz) {
	ALOGD("[%s] ++\n", __FUNCTION__);
	int ret = -1;   //error

	ret = CUSTHAL::getInstance()->openBGMDevice();
	ALOGV("[%s]: Bgm_power_on function ret[%d]\n", __FUNCTION__, ret);
	jboolean result = false;
	if(ret<0)
	{
		result=false;
	}
	else
	{
		result=true;
	}
	ALOGD("[%s] --\n", __FUNCTION__);
	return result;
}
static jint Bgm_power_off(JNIEnv *env, jclass clazz) {
    ALOGD("[%s] ++\n", __FUNCTION__);
    int ret = -1;   //error

    ret = CUSTHAL::getInstance()->closeBGMDevice();
    ALOGV("[%s]: Bgm_power_off ret[%d]\n", __FUNCTION__, ret);
	jboolean result = false;
	if(ret<0)
	{
		result=false;
	}
	else
	{
		result=true;
	}
    ALOGD("[%s] --\n", __FUNCTION__);
	return result;
}
static jint Bgm_power_on_reset(JNIEnv *env, jclass clazz) {
    ALOGD("[%s] ++\n", __FUNCTION__);
    int ret = -1;   //error

    ret = CUSTHAL::getInstance()->powerResetBGM();
    ALOGV("[%s]: Bgm_power_on_reset function ret[%d]\n", __FUNCTION__, ret);
	jboolean result = false;
	if(ret<0)
	{
		result=false;
	}
	else
	{
		result=true;
	}
    ALOGD("[%s] --\n", __FUNCTION__);
	return result;
}
static jint Bgm_wakeup(JNIEnv *env, jclass clazz) {
    ALOGD("[%s] ++\n", __FUNCTION__);
    int ret = -1;   //error

    ret = CUSTHAL::getInstance()->wake_BGM();
    ALOGV("[%s]: Bgm_power_wakeup function ret[%d]\n", __FUNCTION__, ret);
	jboolean result = false;
	if(ret<0)
	{
		result=false;
	}
	else
	{
		result=true;
	}
    ALOGD("[%s] --\n", __FUNCTION__);
	return result;
}
static jint Bgm_Check_RDY(JNIEnv *env, jclass clazz) {
    ALOGD("[%s] ++\n", __FUNCTION__);
    int ret = -1;   //error

    ret = CUSTHAL::getInstance()->checkBGMRDY();
    ALOGV("[%s]: Bgm_Check_RDY function ret[%d]\n", __FUNCTION__, ret);
    if(ret == 1)
    {
    	ret = 0xffff;
    }
    else
    {
    	ret = 0x0000;
    }
    ALOGD("[%s] --\n", __FUNCTION__);
    return ret;
}

//*****************************************************************************
// DESCRIPTION: jni functiion mapping table (JNINativeMethod)
//
//*****************************************************************************
static 	JNINativeMethod gMethods[] = {
	{ "PowerOnBgm", "()Z", (void*) Bgm_power_on },
	{ "PowerOffBgm", "()Z", (void*) Bgm_power_off },
	{ "ResetBgm", "()Z", (void*) Bgm_power_on_reset },
	{ "WakeupBgm", "()Z", (void*) Bgm_wakeup },
	{ "CheckRdyPin", "()I", (void*) Bgm_Check_RDY }
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
int register_custframeworkservice_BgmHWControlService(JNIEnv* env)
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


