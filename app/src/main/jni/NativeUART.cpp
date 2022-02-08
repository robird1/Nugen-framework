//********** COPYRIGHT 2015 altek Corporation *********************************
// FILE NAME:
// VERSION:     $Revision: 19842 $
// DESCRIPTION:
//
//
//*****************************************************************************
// UPDATE LOG:
// $Log: $
//*****************************************************************************
//****************** STANDARD CPP-INCLUDE FILES *********************************
//************* GLOBAL AND PROJECT INCLUDE FILES ******************************
#include <NativeUART.h>

//This macro is used to switch show/disable logcat
#define LOG_NDEBUG 0
// tag in logcat
#define LOG_TAG "NativeUART"

/**
 * NativeUART Constructor does nothing here, but protecting the constructor
 * meets Sigleton pattern.
 * 
 * @param None    [in]
 * @return None [out]
 */
	NativeUART::NativeUART()
    {
    	ALOGI("NativeUART constructor");
    }

/**
 * NativeUART Destructor that does nothing here.
 * 
 * @param None    [in]
 * @return None [out]
 */
    NativeUART::~NativeUART()
    {
    	ALOGI("NativeUART deconstructor");
    }

/**
 * Get NativeUART instance for implementing Singleton pattern.
 * 
 * @param None    [in]
 * @return None [out]
 */
    NativeUART* NativeUART::getInstance()
	{
		static NativeUART *pInstance = new NativeUART;

		return pInstance;
	}

/**
 * Open UART port according to port's configure.
 * 
 * @return int [out] Return 0 if opening succeed. Otherwise, return -1.
 * Range:
 * 0: setting succeed
 * -1: fail to open
 * Unit: N/A
 * Scaling: N/A
 * 
 * @param port    [in] the integer index of a port. (COMMS: 0x000F, BGM: 0x0033)
 * Range: COMMS: 0x000F; BGM: 0x0033
 * Unit: N/A
 * Scaling: N/A
 * @param nUARTFd    [out] the file descriptor of the port.
 * Range: 1 ... Integer max value
 * Unit: N/A
 * Scaling: N/A
 */
	int NativeUART::UART_Open(int nPort, int* nUARTFd)
	{
		int error = -1;

		char* pUART = NULL;

		unsigned int tcResult = 0;

		struct termios termOptions;
		speed_t uartBaudRate;

		switch (nPort)
		{
		  case COMMS_UART:
			  pUART = (char*) COMMS_UART_PATH;

			  UART_GetBLEConfiguration(&termOptions, &uartBaudRate);
			break;

		  case BGM_UART:
			  pUART = (char*) BGM_UART_PATH;

			  UART_GetBGMConfiguration(&termOptions, &uartBaudRate);
			break;

		  default:
			  error = -1;
			  pUART = NULL;
			break;
		}

		if(pUART != NULL)
		{
			*nUARTFd = open(pUART, O_RDWR | O_NOCTTY);

			if (*nUARTFd != -1)
			{
				// set configure
				error = UART_SetConfiguration(*nUARTFd, uartBaudRate, &termOptions);
			}
			else
			{
				// fail open port
				error = -1;
			}
		}
		else
		{
			error = -1;
		}

		return error;
	}

/**
 * Close UART port.
 * 
 * @return int [out] Return 0 if closing succeed. Otherwise, return -1.
 * Range:
 * 0: closing succeed
 * -1: fail to close
 * Unit: N/A
 * Scaling: N/A
 * 
 * @param fd    [in] the file descriptor of a port.
 * Range: 1 ... Integer max value
 * Unit: N/A
 * Scaling: N/A
 */
	int NativeUART::UART_Close(int nUARTFd)
	{
		int error = close(nUARTFd);

		ALOGD("UART_Close: fd=%d, err=%d", nUARTFd, error);

		return error;
	}

/**
 * Write data to a port.
 * 
 * @return int [out] Return 0 if sending succeed. Otherwise, return -1.
 * Range:
 * 0: sending succeed
 * -1: fail to send
 * Unit: N/A
 * Scaling: N/A
 * 
 * @param fd    [in] the file descriptor of a port.
 * Range: 1 ... Integer max value
 * Unit: N/A
 * Scaling: N/A
 * @param length    [in] data length
 * Range: 1 ... unlimited
 * Unit: byte
 * Scaling: N/A
 * @param data    [in] data byte array
 * byte array constraints:
 * Range: 1 ... unlimited
 * Unit: byte
 * Scaling: N/A
 */
	int NativeUART::UART_Write(int fd, const int dataLength, unsigned char* data)
	{
		 int error = -1;

		 if(fd >= 0)
		 {
			const int errorLength = 55;

			unsigned int writeCount = 0;
			int expectWriteLength = dataLength;
			int correctWriteLength = 0;

			for(writeCount = 0; writeCount < dataLength; writeCount += correctWriteLength)
			{
				correctWriteLength = write(fd, data + correctWriteLength, expectWriteLength);

				 if(correctWriteLength >= 0)
				 {
					 expectWriteLength = dataLength - writeCount;
				 }
				 else
				 {
					 // write error
					 ALOGD("UART_Write error!");
					 writeCount = dataLength + errorLength;
				 }
			}

			if(writeCount == dataLength)
			{
				error = 0;
			}
			else
			{
				// write error
				error = -1;
			}

		 }
		 else
		 {
			 // fd error
			 error = -1;
		 }

		 return error;
	}

/**
 * Read data to a port.
 * 
 * @return int [out] Return 0 if reading succeed. Otherwise, return -1.
 * Range:
 * 0: reading succeed
 * -1: fail to read
 * Unit: N/A
 * Scaling: N/A
 * 
 * @param fd    [in] the file descriptor of a port.
 * Range: 1 ... Integer max value
 * Unit: N/A
 * Scaling: N/A
 * @param length    [in] max data length
 * Range: 1 ... Integer max value
 * Unit: byte
 * Scaling: N/A
 * @param data    [in] a byte array buffer
 * byte array constraints:
 * Range: 1 ... max data length
 * Unit: byte
 * Scaling: N/A
 */
	int NativeUART::UART_Read(int fd, int dataLength, unsigned char* data)
	{
		int error = -1;

    	 if(fd >= 0)
		 {
			int expectReadLength = dataLength;
			int correctReadLength = -1;

			correctReadLength = read(fd, data, expectReadLength);

	//		ALOGI("UART_Read: length=%d", correctReadLength);

			 if(correctReadLength >= 0)
			 {
				 // success
				 error = correctReadLength;
			 }
			 else
			 {
				 // read error
				 error = -1;
			 }
		 }
		 else
		 {
			 // fd error
			 error = -1;
		 }

		 return error;
	}

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
 * @return int [out] Return 0 if setting succeed. Otherwise, return -1.
 * Range:
 * 0: setting success
 * -1: fail to set
 * Unit: N/A
 * Scaling: N/A
 * 
 * @param uartFd    [in] the file descriptor of a UART port
 * Range: 1 ... Integer max value
 * Unit: N/A
 * Scaling: N/A
 * @param baudrate    [in] speetd_t of transportation speed
 * Range: B9600 or B230400
 * Unit: speetd_t
 * Scaling: N/A
 * @param srcTermOptions    [in] termios that should be set. For detail of this
 * structure, see termios.
 */
	int NativeUART::UART_SetConfiguration(int uartFd, speed_t baudrate, termios *srcTermOptions)
	{
		int error = -1;

		struct termios dstTermOptions;
		speed_t termOptions;
		int tcResult = 0;

		// clear the flags of fd
		fcntl(uartFd, F_SETFL, 0);

		// Get the current options
		tcResult = tcgetattr(uartFd, &dstTermOptions);

		if(tcResult >= 0)
		{
			// Set 8bit data, No parity, stop 1 bit (8N1):
			dstTermOptions.c_cflag &= (*srcTermOptions).c_cflag;

			// Raw mode
			dstTermOptions.c_iflag &= (*srcTermOptions).c_iflag;

			// raw input
			dstTermOptions.c_lflag &= (*srcTermOptions).c_lflag;

			// raw output
			dstTermOptions.c_oflag &= (*srcTermOptions).c_oflag;

			// clear input buffer
			tcflush(uartFd,TCIFLUSH);

			// inter-character timer unused
			dstTermOptions.c_cc[VTIME] = (*srcTermOptions).c_cc[VTIME];

			// blocking read until 0 character arrives
			dstTermOptions.c_cc[VMIN] = (*srcTermOptions).c_cc[VMIN];

			cfsetispeed(&dstTermOptions, baudrate);
			cfsetospeed(&dstTermOptions, baudrate);

			// Set the new options for the port...
			tcResult = tcsetattr(uartFd, TCSANOW, &dstTermOptions);

			if(tcResult >= 0)
			{
				tcResult = tcflush(uartFd, TCIOFLUSH);

				if (tcResult >= 0)
				{
					error = 0;

					usleep(10000);
				}
				else
				{
					error = -1;
				}
			}
			else
			{
				error = -1;
			}
		}
		else
		{
			error = -1;
		}

		return error;
	}

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
 * @return None [out]
 * 
 * @param termOptions    [in] termios that store the setting. For detail of this
 * structure, see termios.
 * @param baudrate    [in] speetd_t of transportation speed
 * Range: B9600
 * Unit: speetd_t
 * Scaling: N/A
 */
	void NativeUART::UART_GetBGMConfiguration(termios *termOptions, speed_t *baudRate)
	{
		*baudRate = B9600;

		// Set 8bit data, No parity, stop 1 bit (8N1):
		(*termOptions).c_cflag &= ~PARENB;
		(*termOptions).c_cflag &= ~CSTOPB;
		(*termOptions).c_cflag &= ~CSIZE;
		(*termOptions).c_cflag |= CS8 | CLOCAL | CREAD;

		// Raw mode
		(*termOptions).c_iflag &= ~(INLCR | ICRNL | IXON | IXOFF | IXANY);

		// raw input
		(*termOptions).c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);

		// raw output
		(*termOptions).c_oflag &= ~OPOST;

		// inter-character timer unused
		(*termOptions).c_cc[VTIME] = 0;

		// blocking read until 0 character arrives
		(*termOptions).c_cc[VMIN] = 0;



		(*termOptions).c_cflag |= CLOCAL;

		// Disable hardware flow control
		(*termOptions).c_cflag &= ~CRTSCTS;
	}

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
 * @return None [out]
 * 
 * @param termOptions    [in] termios that store the setting. For detail of this
 * structure, see termios.
 * @param baudrate    [in] speetd_t of transportation speed
 * Range: B230400
 * Unit: speetd_t
 * Scaling: N/A
 */
	void NativeUART::UART_GetBLEConfiguration(termios *termOptions, speed_t *baudRate)
	{
		*baudRate = B230400;

		// Set 8bit data, No parity, stop 1 bit (8N1):
		(*termOptions).c_cflag &= ~PARENB;
		(*termOptions).c_cflag &= ~CSTOPB;
		(*termOptions).c_cflag &= ~CSIZE;
		(*termOptions).c_cflag |= CS8 | CLOCAL | CREAD;

		// Raw mode
		(*termOptions).c_iflag &= ~(INLCR | ICRNL | IXON | IXOFF | IXANY);

		// raw input
		(*termOptions).c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);

		// raw output
		(*termOptions).c_oflag &= ~OPOST;

		// inter-character timer unused
		(*termOptions).c_cc[VTIME] = 0;

		// blocking read until 0 character arrives
		(*termOptions).c_cc[VMIN] = 0;


		(*termOptions).c_cflag |= CLOCAL;


		// Enable hardware flow control
		(*termOptions).c_cflag |= CRTSCTS;
	}

