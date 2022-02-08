///////////////////////////////////////////////////////////
//  NativeLED.h
//  Implementation of the Class NativeLED
//  Created on:      24-2-2015 �W�� 11:27:48
//  Original author: ChristinaJiang
///////////////////////////////////////////////////////////

#ifndef I2CCONTROL_H
#define I2CCONTROL_H

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <android/log.h>

#define LIBRARY_B 0x02
#define DEFAULT_DRIVE_TIME 0x13

/* i2c_smbus_xfer read or write markers */
#define I2C_SMBUS_READ	1
#define I2C_SMBUS_WRITE	0

/* SMBus transaction types (size parameter in the above functions)
   Note: these no longer correspond to the (arbitrary) PIIX4 internal codes! */
#define I2C_SMBUS_BYTE_DATA	    2

/* Use this slave address */
#define I2C_SLAVE	0x0703

/* Use this slave address, even if it is already in use by a driver! */
#define I2C_SLAVE_FORCE	0x0706

/* SMBus transfer */
#define I2C_SMBUS	0x0720

/* As specified in SMBus standard */
#define I2C_SMBUS_BLOCK_MAX	32

typedef union HAPTIC_un_smbusData
{
	__u8 byte;
	__u16 word;

	 /* block[0] is used for length
	 and one more for user-space compatibility*/
	__u8 block[I2C_SMBUS_BLOCK_MAX + 2];
} HAPTIC_un_SmbusData;


/* This is the structure as used in the I2C_SMBUS ioctl call */
typedef struct HAPTIC_ss_ioctlDATA
{
	__u8 read_write;
	__u8 command;
	__u32 size;
	HAPTIC_un_SmbusData __user *data;
} HAPTIC_st_IOCTLDATA;

class NativeI2CControl
{
	public:
		/**
		 * NativeI2CControl Destructor that does nothing here.
		 *
		 * @param None    [in]
		 * @return None [out]
		 */
		virtual ~NativeI2CControl();

		/**
		 * Get NativeI2CControl instance for implementing Singleton pattern.
		 *
		 * @param None    [in]
		 * @return None [out]
		 */
		static NativeI2CControl* getInstance();

		/**
		 * Given path string and address, open Haptic device.
		 * Open device via I2C control
		 *
		 * @param pathStr [in] Haptic device path.
		 * Range:
		 * HAPTIC_PATH: "/dev/i2c-1"
		 * Unit: N/A
		 * Scaling: N/A
		 * @param slave_addr [in] address of haptic device
		 * Range:
		 * HAPTIC_SLAVE_BASE_ADDRESS: 0x5a
		 * Unit: N/A
		 * Scaling: N/A
		 * @return int [out] Return negative integer number if there is any error.
		 * Otherwise, return file descriptor.
		 * Range:
		 * 1 ... man integer number: file descriptor
		 * -1: fail to I/O control I2C device
		 * Unit: N/A
		 * Scaling: N/A
		 */
		int HapticI2C_OpenDevice(const char* pathStr, unsigned int slave_addr);

		/**
		 * Given register index and value, set the value into the register.
		 * In order to send data via I2C control, the register index and value are encapsulated
		 * according to I2C data format.
		 *
		 * @param fd    [in] file descriptor of haptic device.
		 * Range:
		 * 1 ... man integer number: file descriptor
		 * Unit: N/A
		 * Scaling: N/A
		 * @param reg   [in] unsigned char variable that is register index in HAPTIC_ee_register.
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
		 * @param value [in] unsigned char variable that is filled register value.
		 * Range:
		 * 0 ... 255
		 * Unit: N/A
		 * Scaling: N/A
		 * @return int [out] Return negative integer number if there is any error.
		 * Otherwise, return 0.
		 * Range:
		 * 0: succeed to write value to register
		 * -1: fail to write value to register
		 * Unit: N/A
		 * Scaling: N/A
		 */
		int HapticI2C_WriteRegister(int fd, unsigned char reg, unsigned char value);

		/**
		 * Given register index, read the value from the register.
		 * In order to send data via I2C control, the register index is encapsulated
		 * according to I2C data format.
		 *
		 * @param fd    [in] file descriptor of haptic device.
		 * Range:
		 * 1 ... man integer number: file descriptor
		 * Unit: N/A
		 * Scaling: N/A
		 * @param reg   [in] unsigned char variable that is register index.
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
		 * @param readValue [out] unsigned char variable that is read from the register.
		 * Range:
		 * 0 ... 255
		 * Unit: N/A
		 * Scaling: N/A
		 * @return int [out] Return negative integer number if there is any error.
		 * Otherwise, return 0.
		 * Range:
		 * 0: succeed to read value to register
		 * -1: fail to read value to register
		 * Unit: N/A
		 * Scaling: N/A
		 */
		int HapticI2C_ReadRegister(int fd, unsigned char reg, unsigned char *readValue);

	private:
		/**
		 * NativeI2CControl Constructor does nothing here, but protecting the constructor
		 * meets Sigleton pattern.
		 *
		 * @param None    [in]
		 * @return None [out]
		 */
		NativeI2CControl();

		/**
		 * NativeI2CControl copy does nothing here, but protecting the function
		 * meets Sigleton pattern.
		 *
		 * @param None    [in]
		 * @return None [out]
		 */
//		NativeI2CControl(const NativeI2CControl&);

};
#endif // I2CCONTROL_H
