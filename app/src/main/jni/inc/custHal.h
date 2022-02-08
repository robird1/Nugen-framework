//********** COPYRIGHT 2013 altek Corporation *********************************
// FILE NAME:   custHal.h
// VERSION:     $Revision: 19842 $
// DESCRIPTION: This header file contains definitions for the ADS7924 driver
//*****************************************************************************
// UPDATE LOG:
// $Log: $
//
//*****************************************************************************
#ifndef __CUSTHAL_H__
#define __CUSTHAL_H__

typedef struct {
	unsigned char cAlarmStatus;			// The alarm status.  [3:0]: Each bit mapping to each channel.
    float	fChannelValue[4];			// Channel 0...3 value.
} ADC_INT_RESULT, *PADC_INT_RESULT;


class CUSTHAL
{
public:
	virtual ~CUSTHAL();
	static CUSTHAL *getInstance();

	int initCustDevice();
	int openCommsDevice();
	int closeCommsDevice();

	// haptic
	int initHaptic();
	int startHaptic();

	// ADC
	int getADCValue(int  nADCID, float* pfValue);
	int getInterruptEventData(ADC_INT_RESULT* data);

	// UICP
	int setDTR(int nValue);
	int setRTS(int nValue);
	int getDSR(unsigned int *nValue);
	int getCTS(unsigned int *nValue);
	int setGPIOMode();
	int setFunctionMode();

	//Bgm
	int	openBGMDevice();
	int	closeBGMDevice();
	int	checkBGMRDY();
	int	wake_BGM();
	int powerResetBGM();

private:
	CUSTHAL();
	void *mHandle;
};

#endif //__CUSTHAL_H__
