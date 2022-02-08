package com.accu_chek.solo_m.rcapp.application.ble.constant;

/**
 * @author EDLiu
 * @version 1.0
 * @created 27-Jan-2015 1:39:54 PM
 */
public final class BlueConstant {

	public static final int BD_ADDR_LEN = 6;
	public static final int MAX_GATT_BUFFER_SIZE = 30;
	
	/**
	 * Comms subsystem SIZE_OF_UUID128
	 */
	public static final int UUID128_LEN		= 16;
	
	/**
	 * blank offset
	 *
	 */
	public static final int BLANK_OFFSET	= 0x00;
	
	public static final class HANDLE
	{
		/**
		 * blank handle
		 */
		public static final int BLANK_HANDLE	= 0x0000;
		public static final int MIN_HANDLE		= 0x0001;
		public static final int MAX_HANDLE		= 0xffff;
	}
	
	
	/**
	 * The device type are SoloM and CGM
	 */
	public static final class DeviceType
	{
		public static final int SOLOM	= 0x00;
		//public static final int CGM		= 0x01;
	}

	
	/**
	 * The TBlueAPI_Cause defines a generic enumeration for cause values. 
	 * Different messages use different subsets of the causes defined herein. The active subset is defined for each message in a separate table as part of the message description.
	 */
	public static final class Cause {
	    
		/**
		 * Indicates that an operation is completed successfully.
		 */
		public static final int SUCCESS 			= 0x00;
		
		/**
		 * Indicates that an operation is accepted.
		 */
		public static final int ACCEPT 				= 0x01;
		
		/**
		 * Indicates that an operation is rejected.
		 */
		public static final int REJECT 				= 0x02;
		
		/**
		 * Indicates that an operation could not be completed due to resource constraints
		 */
		public static final int RESOURCE_ERROR 		= 0x03;
		public static final int INVALID_PARAM 		= 0x04;
		public static final int INVALID_STATE 		= 0x05;
		public static final int CONN_DISCONNECTED 	= 0x06;
		public static final int CONNECTION_PAUSED	= 0x07;
		public static final int CONNECTION_LOST 	= 0x08;
		public static final int AUTH_FAIL 			= 0x09;
		public static final int FLOW_VIOLATION		= 0x0A;
		public static final int INIT_TIMEOUT 		= 0x0B;
		public static final int INIT_OUTOF_SYNC 	= 0x0C;
		public static final int INIT_HW_FAIL 		= 0x0D;
		public static final int LOW_LAYER_ERR 		= 0x0E;
		public static final int ADDR_NOT_RESOLVE 	= 0x0F;
		public static final int UNSPECIFIC 			= 0xFD;
		public static final int NOT_SUPPORTED 		= 0xFE;


	}

	
	/**
	 * BlueAPI_LEAdvType
	 */
	public static final class AdvType
	{
		/**
		 * Connectable undirected advertising
		 * Note: this mode shall only be used for test and verification purposes
		 */
		public static final int UNDIRECTED		= 0x00;
		
		/**
		 * Connectable directed advertising
		 * Note: this mode shall only be used for test and verification purposes
		 */
		public static final int DIRECTED		= 0x01;
		
		/**
		 * Scanable undirected advertising
		 */
		public static final int SCANABLE		= 0x02;
		
		/**
		 * Non-connectable undirected advertising
		 */
		public static final int NONCONNECTABLE	= 0x03;
		
		/**
		 * Scan Response
		 */
		public static final int SCANRESPONSE	= 0x04;
		
	}
	/**
	 * TBlueAPI_LEScanMode
	 */
	public static final class ScanMode
	{
		/**
		 * Scan disabled
		 */
		public static final int DISABLE = 0x00;
		
		/**
		 * Passive scan enabled
		 */
		public static final int PASSIVE = 0x01;
		
		/**
		 * Active scan enabled
		 */
		public static final int ACTIVE = 0x02;
		
	}
	/**
	 * UUID of reading device info
	 *
	 */
	public static final class DeviceInfo
	{
		/**
		 * UUID for reading manufacturer
		 */
		public static final int UUID_MFG 	= 0x2A29;
		
		/**
		 * UUID for reading model name
		 */
		public static final int UUID_MODEL 	= 0x2A24;
		
		/**
		 * UUID for reading Serial number
		 */
		public static final int UUID_SERIAL = 0x2A25;
		
		public static final int UUID_FW 	= 0x2A26;
		public static final int UUID_HW 	= 0x2A27;
		public static final int UUID_SW 	= 0x2A28;
		public static final int UUID_SYSID 	= 0x2A23;
		
	}
	/**
	 * TBlueAPI_GATTDiscoveryType
	 */
	public static final class DiscoveryType {

		/**
		 * Discover all primary services
		 */
		public static final short SERVICE = 0x01;
		
		/**
		 * Discover service by UUID
		 */
		public static final short SERVICE_BY_UUID = 0x02;
		
		/**
		 * Discover all characteristics
		 */
		public static final short CHARC = 0x03;
		
		/**
		 * Discover all characteristic descriptors
		 */
		public static final short CHARC_DESC = 0x04;
		
		/**
		 * Discover relationship (included services)
		 */
		public static final short RELATIONSHIP = 0x05;

	
	}

	/**
	 * BlueAPI TBlueAPI_GATTWriteType
	 * @author EDLiu
	 *
	 */
	public static final class WriteType
	{
		/**
		 * ATT ¡§Write Request¡¨
		 */
		public static final int REQUEST = 0x01;
		
		/**
		 * ATT ¡§Write Command¡¨
		 */
		public static final int COMMAND = 0x02;
		
		/**
		 * ATT "Prepare Write Request"
		 */
		public static final int PREPARE_REQUEST = 0x03;
		
	}
	
	/**
	 * TBlueAPI_RemoteBDType
	 */
	public static final class RemoteBDType
	{
		/**
		 * Classic BR/EDR Bluetooth address
		 */
		public static final int CLASSIC = 0x00;
		
		/**
		 * Bluetooth low energy public address
		 */
		public static final int BLE_PUBLIC = 0x02;
		
		/**
		 * Bluetooth low energy random address
		 */
		public static final int BLE_RANDOM = 0x03;
		
		/**
		 * Any Bluetooth address
		 */
		public static final int ANY = 0x04;
		
		/**
		 * Bluetooth low energy peer with resolvable private address that distributed it¡¦s IRK.
		 */
		public static final int BLE_RESOLVED = 0x0A;
		
	}
	/**
	 * BlueAPI TBlueAPI_GATTReadType
	 *
	 */
	public static final class ReadType
	{
		/**
		 * ATT ¡§Read (Blob) Request¡¨
		 */
		public static final int BASIC = 0x01;
		
		/**
		 * ATT ¡§Read By Type Request¡¨
		 */
		public static final int UUID = 0x02;
		
	}
	/**
	 * TBlueAPI_InternalEventType
	 */
	public static final class InternalEvent
	{
		/**
		 * The MDC received a CreateMDLConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidCreateMDLConf		=	0x01;
		
		/**
		 * The MDC received a ReconnectMDLConf message from the application that could not be handled and that will be ignored 
		 */
		public static final int InvalidReconnectMDLConf		=	0x02;
		
		/**
		 * The MDC received a DisconnectMDLConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidDisconnectMDLConf	=	0x03;
		
		/**
		 * The MDC received a DataConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidDataConf				=	0x04;
		
		/**
		 * The MDC received a DIDDeviceConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidDIDDeviceConf		=	0x05;
		
		/**
		 * The MDC received a HDPServiceConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidHDPServiceConf		=	0x06;
		
		/**
		 * The MDC received a HDPEndpointConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidHDPEndpointConf		=	0x07;
		
		/**
		 * The MDC received a SPPEndpointConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidSPPEndpointConf		=	0x08;
		
		/**
		 * The MDC received a security related confirmation message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidSecurityConf			=	0x09;
		
		/**
		 * The MDC received an invalid event or discovered an invalid behavior from / of the remote device that can affect the functionality of the connection. The reaction of the MDC on the air interface will be according to the respective specifications ([X1], [X2], [X3], [X4],[X5])
		 */
		public static final int InvalidRemoteEvent			=	0x0A;
		
		/**
		 * The MDC implements several timeouts that prevent the system to be driven into a deadlock situation by a nonfunctional MDH or remote device. This timeout should never occur during normal operation
		 */
		public static final int CommunicationTimeout		=	0x0B;
		
		/**
		 * The MDC received a GATTAttributeReadConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidGATTAttributeReadConf	=	0x0C;
		
		/**
		 * The MDC received a GATTAttributeWriteConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidGATTAttributeWriteConf	=	0x0D;
		
		/**
		 * The MDC received a GATTDiscoveryConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidGATTDiscoveryConf	=	0x0E;
		
		/**
		 * The MDC received a GATTAttributeConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidGATTAttributeConf	=	0x0F;
		
		/**
		 * The MDC received a LEConnectionUpdateConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidConnectionUpdateConf	=	0x10;
		
		/**
		 * The MDC received a GATTSDPDiscoveryConf message from the application that could not be handled and that will be ignored
		 */
		public static final int InvalidGATTSDPDiscoveryConf	=	0x11;
		

	}

}// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
