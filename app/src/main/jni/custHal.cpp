//********** COPYRIGHT 2013 altek Corporation *********************************
// FILE NAME:   custHal.cpp
// VERSION:     $Revision: 19842 $
// DESCRIPTION: For control customize hardware
//
//
//*****************************************************************************
// UPDATE LOG:
// $Log: $

#define LOG_TAG "custHal"

#include <cstddef>
#include <dlfcn.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <custHal.h>

#include <android/log.h>


#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )

//=================================================================
//class CUSTHAL definition
//=================================================================
//constructor
CUSTHAL::CUSTHAL()
{
	ALOGV("[%s]: enter\n",__FUNCTION__);
	const char* path = "/system/lib/libaltek_utils.so";
	mHandle = dlopen(path, RTLD_NOW);

	if (mHandle == NULL) {
		char const *err_str = dlerror();
	    ALOGE("load: module=%s\n%s", path, err_str?err_str:"unknown");
	}

	//reset
	dlerror();
	ALOGV("[%s]: exit\n",__FUNCTION__);
}

//destructor
CUSTHAL::~CUSTHAL()
{
	ALOGV("[%s]: enter\n",__FUNCTION__);

	if(mHandle != NULL) {
		dlclose(mHandle);
		mHandle = NULL;
	}

	ALOGV("[%s]: exit\n",__FUNCTION__);
}

//singletone
CUSTHAL *CUSTHAL::getInstance()
{
	static CUSTHAL* pinstance = new CUSTHAL;
	return pinstance;
}

//initial device
int CUSTHAL::initCustDevice()
{
	ALOGV("[%s]: enter\n",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "AltekHardware_Init";
	int (*pInit)(void) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	pInit = (int(*)(void))dlsym(mHandle, funcName);

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

//power on comms
int CUSTHAL::openCommsDevice()
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "COMMS_OpenDevice";
	int (*pOpen)(void) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	pOpen = (int(*)(void))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*pOpen)();
		ALOGV("[%s]: function call sucessful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}

//power off comms
int CUSTHAL::closeCommsDevice()
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "COMMS_CloseDevice";
	int (*pOpen)(void) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	pOpen = (int(*)(void))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*pOpen)();
		ALOGV("[%s]: function call sucessful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}

//power on BGM
int CUSTHAL::openBGMDevice()
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "BGM_OpenDevice";
	int (*pOpen)(void) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	pOpen = (int(*)(void))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*pOpen)();
		ALOGV("[%s]: function call sucessful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}
//power off BGM
int CUSTHAL::closeBGMDevice()
{
  ALOGV("[%s]: enter",__FUNCTION__);
  int ret = -1;
  const char *error = NULL;
  const char* funcName = "BGM_CloseDevice";
  int (*pOpen)(void) = NULL;

  if(mHandle == NULL) {
      ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
      goto DOWN;
  }

  pOpen = (int(*)(void))dlsym(mHandle, funcName);

  if ((error = dlerror()) != NULL)  {
      ALOGE("[%s]: %s\n", __FUNCTION__, error);
  }
  else {
      ret = (*pOpen)();
      ALOGV("[%s]: function call sucessful\n",__FUNCTION__);
  }

DOWN:
  ALOGV("[%s]: exit",__FUNCTION__);
  return ret;
}
//check BGM RDY pin
int CUSTHAL::checkBGMRDY()
{
  ALOGV("[%s]: enter",__FUNCTION__);
  int ret = -1;
  const char *error = NULL;
  const char* funcName = "BGM_CheckReady";
  int (*pOpen)(void) = NULL;

  if(mHandle == NULL) {
      ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
      goto DOWN;
  }

  pOpen = (int(*)(void))dlsym(mHandle, funcName);

  if ((error = dlerror()) != NULL)  {
      ALOGE("[%s]: %s\n", __FUNCTION__, error);
  }
  else {
      ret = (*pOpen)();
      ALOGV("[%s]: function call sucessful\n",__FUNCTION__);
  }

DOWN:
  ALOGV("[%s]: exit",__FUNCTION__);
  return ret;
}
//wakeup BGM
int CUSTHAL::wake_BGM()
{
  ALOGV("[%s]: enter",__FUNCTION__);
  int ret = -1;
  const char *error = NULL;
  const char* funcName = "BGM_WakeUp";
  int (*pOpen)(void) = NULL;

  if(mHandle == NULL) {
      ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
      goto DOWN;
  }

  pOpen = (int(*)(void))dlsym(mHandle, funcName);

  if ((error = dlerror()) != NULL)  {
      ALOGE("[%s]: %s\n", __FUNCTION__, error);
  }
  else {
      ret = (*pOpen)();
      ALOGV("[%s]: function call sucessful\n",__FUNCTION__);
  }

DOWN:
  ALOGV("[%s]: exit",__FUNCTION__);
  return ret;
}
//power on reset BGM
int CUSTHAL::powerResetBGM()
{
  ALOGV("[%s]: enter",__FUNCTION__);
  int ret = -1;
  const char *error = NULL;
  const char* funcName = "BGM_PowerOnReset";
  int (*pOpen)(void) = NULL;

  if(mHandle == NULL) {
      ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
      goto DOWN;
  }

  pOpen = (int(*)(void))dlsym(mHandle, funcName);

  if ((error = dlerror()) != NULL)  {
      ALOGE("[%s]: %s\n", __FUNCTION__, error);
  }
  else {
      ret = (*pOpen)();
      ALOGV("[%s]: function call sucessful\n",__FUNCTION__);
  }

DOWN:
  ALOGV("[%s]: exit",__FUNCTION__);
  return ret;
}
// haptic init
int CUSTHAL::initHaptic()
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "HapticInit";
	int (*pOpen)(void) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	pOpen = (int(*)(void))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*pOpen)();
		ALOGV("[%s]: function call sucessful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}

// haptic on
int CUSTHAL::startHaptic()
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "HapticOn";
	int (*pOpen)(void) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	pOpen = (int(*)(void))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*pOpen)();
		ALOGV("[%s]: function call sucessful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}

// read ADC
int CUSTHAL::getADCValue(int  nADCID, float* pfValue)
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "ADC_ReadVoltageValue";
	int (*readADC)(int nADCID, float* pfValue) = NULL;

	float test = 0.0;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	readADC = (int(*)(int, float*))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*readADC)(nADCID, pfValue);
		ALOGV("[%s]: function call successful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}

// read interrupt ADC data
int CUSTHAL::getInterruptEventData(ADC_INT_RESULT *data)
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "ADC_GetInterruptEventData";
	int (*readADC)(ADC_INT_RESULT *data) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	readADC = (int(*)(ADC_INT_RESULT*))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*readADC)(data);
		ALOGV("[%s]: function call successful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}

// set DTR
int CUSTHAL::setDTR(int nValue)
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "COMMS_SetDTR";
	int (*setDTR)(int nValue) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	setDTR = (int(*)(int))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*setDTR)(nValue);
		ALOGV("[%s]: function call successful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}

// set RTS
int CUSTHAL::setRTS(int nValue)
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "COMMS_SetRTS";
	int (*setRTS)(int nValue) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	setRTS = (int(*)(int))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*setRTS)(nValue);
		ALOGV("[%s]: function call successful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}


// get DSR
int CUSTHAL::getDSR(unsigned int *nValue)
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "COMMS_ReadDSR";
	int (*getDSR)(unsigned int *nValue) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	getDSR = (int(*)(unsigned int*))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*getDSR)(nValue);
		ALOGV("[%s]: function call successful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}

// get CTS
int CUSTHAL::getCTS(unsigned int *nValue)
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "COMMS_ReadCTS";
	int (*getCTS)(unsigned int *nValue) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	getCTS = (int(*)(unsigned int*))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*getCTS)(nValue);
		ALOGV("[%s]: function call successful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}

// set GPIO mode
int CUSTHAL::setGPIOMode()
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "COMMS_SetRTSCTS2GPIOMode";
	int (*setGPIO)(void) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	setGPIO = (int(*)(void))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*setGPIO)();
		ALOGV("[%s]: function call successful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}

// set function mode
int CUSTHAL::setFunctionMode()
{
	ALOGV("[%s]: enter",__FUNCTION__);
	int ret = -1;
	const char *error = NULL;
	const char* funcName = "COMMS_SetRTSCTS2FuncMode";
	int (*setFunc)(void) = NULL;

	if(mHandle == NULL) {
		ALOGE("[%s]: mHandle is NULL\n",__FUNCTION__);
		goto DOWN;
	}

	setFunc = (int(*)(void))dlsym(mHandle, funcName);

	if ((error = dlerror()) != NULL)  {
		ALOGE("[%s]: %s\n", __FUNCTION__, error);
	}
	else {
		ret = (*setFunc)();
		ALOGV("[%s]: function call successful\n",__FUNCTION__);
	}

DOWN:
	ALOGV("[%s]: exit",__FUNCTION__);
	return ret;
}
