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
#define LOG_TAG "selftest"

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
static const char* const kClassName =
		"com/accu_chek/solo_m/rcapp/application/selftest/SelftestNative";

static jint SelfTest_PeekInsulinKeyState(JNIEnv *env, jclass clazz) {

	int keystate = -1;
	//	NativeLED *native = NativeLED::getInstance();

	ALOGD("[%s] insulin key state : %d \n", __FUNCTION__ , keystate);

	return keystate;

}
/**
 * Do CPU Test
 *
 *
 * @param env    [in] JNIEnv pointer that is jni environment pointer. JNIEnv is
 * provided by Android SDK.
 *
 * @param clazz    [in] jclass is the object of the java servcie framework. jclass
 * provided by Android SDK.
 *
 * @param add
 *            [in] : ALU parameter.
 *            Range : [0-255]
 *            Unit:int
 *            Scaling:1
 *
 * @param sub
 *            [in] : ALU parameter.
 *            Range : [0-255]
 *            Unit:int
 *            Scaling:1
 *
 * @param mul
 *            [in] : ALU parameter.
 *            Range : [0-255]
 *            Unit:int
 *            Scaling:1
 *
 * @return int [out] Return calculated result of ALU
 *         Range: [0-255]
 *         Unit: int
 *         Scaling:1
 */
static jint SelfTest_CPUTest(JNIEnv* env, jclass clazz, jint add, jint sub,
		jint mul) {
	int testResult = -1;
	//	NativeLED *native = NativeLED::getInstance();

	ALOGV(" SelfTest_CPUTest %d,%d,%d\n",add,sub,mul);

	testResult = (add - sub) * mul;

	return testResult;
}

//*****************************************************************************
// DESCRIPTION: jni functiion mapping table (JNINativeMethod)
//
//*****************************************************************************
static JNINativeMethod gMethods[] =
{
	{ "peekInsulinKeyState", "()I",(void*) SelfTest_PeekInsulinKeyState },
    {"cpuTest",     "(III)I", (void*) SelfTest_CPUTest }

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
int register_custframeworkservice_SelftestService(JNIEnv* env) {

	jclass clazz = NULL;

	/* look up the class */
	clazz = env->FindClass(kClassName);
	if (clazz == NULL) {
		ALOGE("Can't find class %s\n", kClassName);
		return -1;
	}

	/* register all the methods */
	if (env->RegisterNatives(clazz, gMethods, sizeof(gMethods)
			/ sizeof(gMethods[0])) != JNI_OK) {
		ALOGE("Failed registering methods for %s\n", kClassName);
		return -1;
	}

	/* fill out the rest of the ID cache */
	return 0;
}

