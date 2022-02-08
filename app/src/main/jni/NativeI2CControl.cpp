#include <NativeI2CControl.h>

//This macro is used to switch show/disable logcat
#define LOG_TAG "nativeHapticControl"

// Log functions
#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )

/**
 * NativeI2CControl Constructor does nothing here, but protecting the constructor
 * meets Sigleton pattern.
 * 
 * @param None    [in]
 * @return None [out]
 */
NativeI2CControl::NativeI2CControl()
{

}

/**
 * NativeI2CControl Destructor that does nothing here.
 * 
 * @param None    [in]
 * @return None [out]
 */
NativeI2CControl::~NativeI2CControl()
{

}

NativeI2CControl* NativeI2CControl::getInstance()
{
	static NativeI2CControl *pInstance = new NativeI2CControl;

	return pInstance;
}


/**
 * Given path string and address, open Haptic device.
 * Open device via I2C control
 * 
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return file descriptor.
 * Range:
 * 1 ... man integer number: file descriptor
 * -1: fail to I/O control I2C device
 * Unit: N/A
 * Scaling: N/A
 * 
 * @param pathStr    [in] Haptic device path.
 * Range:
 * HAPTIC_PATH: "/dev/i2c-1"
 * Unit: N/A
 * Scaling: N/A
 * @param slave_addr    [in] address of haptic device
 * Range:
 * HAPTIC_SLAVE_BASE_ADDRESS: 0x5a
 * Unit: N/A
 * Scaling: N/A
 */
int NativeI2CControl::HapticI2C_OpenDevice(const char* str, unsigned int slave_addr)
{
	int error = 0;
	int fd = open(str, O_RDWR);

	if (fd < 0)
	{
		ALOGD("%s, open I2C ERROR erron = %d\n", __func__, errno);
	}
	else
	{
		error = ioctl(fd, I2C_SLAVE, slave_addr);
		error |= ioctl(fd, I2C_SLAVE_FORCE, slave_addr);

		if (error < 0)
		{
			ALOGD("%s, open I2C slave device ERROR  erron = %d\n", __func__, errno);

			// reset
			fd = -1;
		}
	}

	return fd;
}

/**
 * Given register index and value, set the value into the register.
 * In order to send data via I2C control, the register index and value are
 * encapsulated
 * according to I2C data format.
 * 
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return 0.
 * Range:
 * 0: succeed to write value to register
 * -1: fail to write value to register
 * Unit: N/A
 * Scaling: N/A
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: N/A
 * @param reg    [in] unsigned char variable that is register index in
 * HAPTIC_ee_register.
 * Range: (in HAPTIC_ee_register)
 * STATUS_REG  = 0x00,
 * MODE_REG = 0x01,
 * REAL_TIME_PLAYBACK_REG = 0x02,
 * LIBRARY_SELECTION_REG = 0x03,
 * WAVEFORM_SEQUENCER_REG = 0x04,
 * GO_REG = 0x0C,
 * OVERDRIVE_TIME_OFFSET_REG = 0x0D,
 * SUSTAIN_TIME_OFFSET_POS_REG = 0x0E,
 * SUSTAIN_TIME_OFFSET_NEG_REG = 0x0F,
 * BRAKE_TIME_OFFSET_REG = 0x10,
 * AUDIO_HAPTICS_CONTROL_REG = 0x11,
 * AUDIO_HAPTICS_MIN_INPUT_REG = 0x12,
 * AUDIO_HAPTICS_MAX_INPUT_REG = 0x13,
 * AUDIO_HAPTICS_MIN_OUTPUT_REG = 0x14,
 * AUDIO_HAPTICS_MAX_OUTPUT_REG = 0x15,
 * RATED_VOLTAGE_REG = 0x16,
 * OVERDRIVE_CLAMP_VOLTAGE_REG = 0x17,
 * AUTO_CALI_RESULT_REG = 0x18,
 * AUTO_CALI_BACK_EMF_RESULT_REG = 0x19,
 * FEEDBACK_CONTROL_REG = 0x1A,
 * Control1_REG = 0x1B,
 * Control2_REG = 0x1C,
 * Control3_REG = 0x1D,
 * AUTOCAL_MEM_INTERFACE_REG = 0x1E
 * Unit: N/A
 * Scaling: N/A
 * @param value    [in] unsigned char variable that is filled register value.
 * Range:
 * 0 ... 255
 * Unit: N/A
 * Scaling: N/A
 */
int NativeI2CControl::HapticI2C_WriteRegister(int fd, unsigned char reg, unsigned char value)
{
    int error = 0;
	HAPTIC_un_SmbusData smbus_data;
	HAPTIC_st_IOCTLDATA ioctl_data;

	smbus_data.byte = value;
	ioctl_data.read_write = I2C_SMBUS_WRITE;
	ioctl_data.command = reg;
	ioctl_data.size = I2C_SMBUS_BYTE_DATA;
	ioctl_data.data = &smbus_data;

	error = ioctl(fd, I2C_SMBUS, &ioctl_data);

	if(error < 0)
	{
		ALOGD("%s, err=%d\n", __func__, error);
	}

	return error;
}

/**
 * Given register index, read the value from the register.
 * In order to send data via I2C control, the register index is encapsulated
 * according to I2C data format.
 * 
 * @return int [out] Return negative integer number if there is any error.
 * Otherwise, return 0.
 * Range:
 * 0: succeed to read value to register
 * -1: fail to read value to register
 * Unit: N/A
 * Scaling: N/A
 * 
 * @param fd    [in] file descriptor of haptic device.
 * Range:
 * 1 ... man integer number: file descriptor
 * Unit: N/A
 * Scaling: N/A
 * @param reg    [in] unsigned char variable that is register index.
 * Range: (in HAPTIC_ee_register)
 * STATUS_REG  = 0x00,
 * MODE_REG = 0x01,
 * REAL_TIME_PLAYBACK_REG = 0x02,
 * LIBRARY_SELECTION_REG = 0x03,
 * WAVEFORM_SEQUENCER_REG = 0x04,
 * GO_REG = 0x0C,
 * OVERDRIVE_TIME_OFFSET_REG = 0x0D,
 * SUSTAIN_TIME_OFFSET_POS_REG = 0x0E,
 * SUSTAIN_TIME_OFFSET_NEG_REG = 0x0F,
 * BRAKE_TIME_OFFSET_REG = 0x10,
 * AUDIO_HAPTICS_CONTROL_REG = 0x11,
 * AUDIO_HAPTICS_MIN_INPUT_REG = 0x12,
 * AUDIO_HAPTICS_MAX_INPUT_REG = 0x13,
 * AUDIO_HAPTICS_MIN_OUTPUT_REG = 0x14,
 * AUDIO_HAPTICS_MAX_OUTPUT_REG = 0x15,
 * RATED_VOLTAGE_REG = 0x16,
 * OVERDRIVE_CLAMP_VOLTAGE_REG = 0x17,
 * AUTO_CALI_RESULT_REG = 0x18,
 * AUTO_CALI_BACK_EMF_RESULT_REG = 0x19,
 * FEEDBACK_CONTROL_REG = 0x1A,
 * Control1_REG = 0x1B,
 * Control2_REG = 0x1C,
 * Control3_REG = 0x1D,
 * AUTOCAL_MEM_INTERFACE_REG = 0x1E
 * Unit: N/A
 * Scaling: N/A
 * @param readValue    [out] unsigned char variable that is read from the register.
 * Range:
 * 0 ... 255
 * Unit: N/A
 * Scaling: N/A
 */
int NativeI2CControl::HapticI2C_ReadRegister(int fd, unsigned char reg, unsigned char *readValue)
{
    int error = -1;
	HAPTIC_un_SmbusData smbus_data = {0};
	HAPTIC_st_IOCTLDATA ioctl_data = {I2C_SMBUS_READ, reg, I2C_SMBUS_BYTE_DATA, &smbus_data};

	error = ioctl(fd, I2C_SMBUS, &ioctl_data);

	if(error < 0)
	{
		ALOGD("%s, err=%d\n", __func__, error);
	}
	else
	{
		*readValue = smbus_data.byte;
	}

    return error;
}
