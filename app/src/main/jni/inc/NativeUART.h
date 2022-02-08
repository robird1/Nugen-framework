#ifndef UART_H
#define UART_H

#include <jni.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <android/log.h>

// Log functions
#define ALOGV(...) __android_log_print( ANDROID_LOG_VERBOSE, LOG_TAG, __VA_ARGS__ )
#define ALOGD(...) __android_log_print( ANDROID_LOG_DEBUG,  LOG_TAG, __VA_ARGS__ )
#define ALOGI(...) __android_log_print( ANDROID_LOG_INFO,  LOG_TAG, __VA_ARGS__ )
#define ALOGW(...) __android_log_print( ANDROID_LOG_WARN,  LOG_TAG, __VA_ARGS__ )
#define ALOGE(...) __android_log_print( ANDROID_LOG_ERROR,  LOG_TAG, __VA_ARGS__ )

// the uart path of COMMS and BGM
#define COMMS_UART_PATH "/dev/ttyMT0"
// the uart path of BGM
#define BGM_UART_PATH "/dev/ttyMT1"

enum NATIVE_UART_DEVICE_INDEX
{
// the index of COMMS and BGM
 COMMS_UART = 0x000F,
// the index of BGM
 BGM_UART = 0x0033
};

class NativeUART
{
public:
	/**
	 * NativeUART Destructor that does nothing here.
	 *
	 * @param None    [in]
	 * @return None [out]
	 */
	virtual ~NativeUART();

	/**
	 * Get NativeUART instance for implementing Singleton pattern.
	 *
	 * @param None    [in]
	 * @return None [out]
	 */
	static NativeUART* getInstance();

	/**
	 * Open UART port according to port's configure.
	 *
	 * @param port    [in] the integer index of a port. (COMMS: 0x000F, BGM: 0x0033)
	 * Range: COMMS: 0x000F; BGM: 0x0033
	 * Unit: N/A
	 * Scaling: 1
	 * @param nUARTFd    [out] the file descriptor of the port.
	 * Range: 1 ... Integer max value
	 * Unit: N/A
	 * Scaling: 1
	 * @return int [out] Return 0 if opening succeed. Otherwise, return -1.
	 * Range:
	 * 0: setting succeed
	 * -1: fail to open
	 * Unit: N/A
	 * Scaling: 1
	 */
	int UART_Open(int port, int* nUARTFd);

	/**
	 * Close UART port.
	 *
	 * @param fd    [in] the file descriptor of a port.
	 * Range: 1 ... Integer max value
	 * Unit: N/A
	 * Scaling: 1
	 * @return int [out] Return 0 if closing succeed. Otherwise, return -1.
	 * Range:
	 * 0: closing succeed
	 * -1: fail to close
	 * Unit: N/A
	 * Scaling: 1
	 */
	int UART_Close(int fd);

	/**
	 * 	Write data to a port.
	 *
	 * @param fd    [in] the file descriptor of a port.
	 * Range: 1 ... Integer max value
	 * Unit: N/A
	 * Scaling: 1
	 * @param length [in] data length
	 * Range: 1 ... unlimited
	 * Unit: byte
	 * Scaling: 1
	 * @param data [in] data byte array
	 * byte array constraints:
	 * Range: 1 ... unlimited
	 * Unit: byte
	 * Scaling: 1
	 * @return int [out] Return 0 if sending succeed. Otherwise, return -1.
	 * Range:
	 * 0: sending succeed
	 * -1: fail to send
	 * Unit: N/A
	 * Scaling: 1
	 */
	int UART_Write(int fd, int length, unsigned char* data);

	/**
	 * Read data to a port.
	 *
	 * @param fd    [in] the file descriptor of a port.
	 * Range: 1 ... Integer max value
	 * Unit: N/A
	 * Scaling: 1
	 * @param length [in] max data length
	 * Range: 1 ... Integer max value
	 * Unit: byte
	 * Scaling: 1
	 * @param data [in] a byte array buffer
	 * byte array constraints:
	 * Range: 1 ... max data length
	 * Unit: byte
	 * Scaling: 1
	 * @return int [out] Return 0 if reading succeed. Otherwise, return -1.
	 * Range:
	 * 0: reading succeed
	 * -1: fail to read
	 * Unit: N/A
	 * Scaling: 1
	 */
	int UART_Read(int fd, int length, unsigned char* data);

private:
	/**
	 * NativeUART Constructor does nothing here, but protecting the constructor
	 * meets Sigleton pattern.
	 *
	 * @param None    [in]
	 * @return None [out]
	 */
	NativeUART();

	/**
	 * NativeUART copy does nothing here, but protecting the function
	 * meets Sigleton pattern.
	 *
	 * @param None    [in]
	 * @return None [out]
	 */
//	NativeUART(const NativeUART&);

	/**
	 * NativeUART copy does nothing here, but protecting the function
	 * meets Sigleton pattern.
	 *
	 * @param None    [in]
	 * @return None [out]
	 */
//	const NativeUART& operator= (const NativeUART&);

	/**
	 * Set UART port's configuration. The setting content is
	 * 1. Set bit data, parity, stop bit
	 * 2. Set mode
	 * 3. Set input
	 * 4. Set output
	 * 5. clear input buffer
	 * 6. Set inter-character timer
	 * 7. Set blocking read
	 * 8. Flow control or not
	 * 9. Baud rate
	 *
	 * @param uartFd    [in] the file descriptor of a UART port
	 * Range: 1 ... Integer max value
	 * Unit: N/A
	 * Scaling: 1
	 * @param baudrate [in] speetd_t of transportation speed
	 * Range: B9600 or B230400
	 * Unit: speetd_t
	 * Scaling: 1
	 * @param srcTermOptions [in] termios that should be set. For detail of this structure, see termios.
	 * @return int [out] Return 0 if setting succeed. Otherwise, return -1.
	 * Range:
	 * 0: setting success
	 * -1: fail to set
	 * Unit: N/A
	 * Scaling: 1
	 */
	int UART_SetConfiguration(int uartFd, speed_t baudrate, termios *srcTermOptions);

	/**
	 * Get BGM configuration. The configuration is
	 * 1. Set 8bit data, No parity, stop 1 bit (8N1)
	 * 2. Raw mode
	 * 3. raw input
	 * 4. raw output
	 * 5. clear input buffer
	 * 6. inter-character timer unused
	 * 7. blocking read until 0 character arrives
	 * 8. Disable hardware flow control
	 * 9. Baud rate: B9600
	 *
	 * @param termOptions [in] termios that store the setting. For detail of this structure, see termios.
	 * @param baudrate [in] speetd_t of transportation speed
	 * Range: B9600
	 * Unit: speetd_t
	 * Scaling: 1
	 * @return None [out]
	 */
	void UART_GetBGMConfiguration(termios *termOptions, speed_t *baudrate);

	/**
	 * Set UART port's configuration. The setting content is
	 * 1. Set 8bit data, No parity, stop 1 bit (8N1)
	 * 2. Raw mode
	 * 3. raw input
	 * 4. raw output
	 * 5. clear input buffer
	 * 6. inter-character timer unused
	 * 7. blocking read until 0 character arrives
	 * 8. Enable hardware flow control
	 * 9. Baud rate: B230400
	 *
	 * @param termOptions [in] termios that store the setting. For detail of this structure, see termios.
	 * @param baudrate [in] speetd_t of transportation speed
	 * Range: B230400
	 * Unit: speetd_t
	 * Scaling: 1
	 * @return None [out]
	 */
	void UART_GetBLEConfiguration(termios *termOptions, speed_t *baudrate);
};

#endif //UART_H
