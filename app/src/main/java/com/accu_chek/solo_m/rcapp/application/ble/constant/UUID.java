package com.accu_chek.solo_m.rcapp.application.ble.constant;


public final class UUID 
{

	
    public static final class UUID16
    {	
    	public static final int BLANK					= 0x0000;
    	public static final int IAS_BEEP_MP             = 0x2A06;
    	
    	public static final int SERVICE_CHANGE          = 0x2A05;
    	
    	public static final int BATTERY_LEVEL           = 0x2A19;
    	public static final int DIS_SYSTEM_ID           = 0x2A23;
    	public static final int DIS_MODEL_NUMBER        = 0x2A24;
    	public static final int DIS_SERIAL_NUMBER       = 0x2A25;
    	public static final int DIS_MANUFACTURER_NAME   = 0x2A29;
    	
    	public static final int RECORD_ACCESS_CP        = 0x2A52;
    	
    	public static final int IDD_STATUS_CHANGED		= 0x2B01;
    	public static final int IDD_DEVICE_STATUS		= 0x2B02;
    	public static final int IDD_FEATURES			= 0x2B03;
    	public static final int IDD_STATUS_READER_CP	= 0x2B04;
    	public static final int IDD_COMMAND_CP			= 0x2B05;
    	public static final int IDD_COMMAND_DATA		= 0x2B06;
    	public static final int IDD_ANNUNCIATION_STATUS = 0x2B07;
    	public static final int IDD_HISTORY_DATA        = 0x2B08;
    	
    	//UUID128 to 16
    	public static final int KEY_EXCHANGE_CP         = 0xBA14;
    	public static final int SOLOM_STATUS_CHANGED    = 0x8AFA;
    	public static final int SOLOM_CP                = 0x45C3;
    }
    
    public static final class UUID128
    {
    	public static final byte[] BLANK =
   		{
		    0x00,  0x00, 0x00,  0x00, 0x00,  0x00, 0x00, 0x00,
		    0x00,  0x00, 0x00,  0x00, 0x00,  0x00, 0x00, 0x00
		};

    	public static final byte[] SOLOM_STATUS_CHANGED =
    		{
    		    (byte) 0x8A,  (byte) 0xFA, 0x20, (byte) 0xFF, 0x32, (byte) 0xEF, 0x47, 0x4C,
    		    (byte) 0xA2, 0x1C, (byte) 0xAC, (byte) 0xD6, (byte) 0xD5, (byte) 0xCE, 0x62, (byte) 0xCD
    		};

    		public static final byte[] SOLOM_EMWR_STATUS =
    		{
    		    0x76, (byte) 0xE2, 0x72, 0x43, (byte) 0xDA, (byte) 0x95, 0x4F, (byte) 0xF4,
    		    (byte) 0xAA, 0x7F, (byte) 0xDA, (byte) 0x8F, (byte) 0xE6, (byte) 0xF1, (byte) 0xE7, 0x2A
    		};

    		public static final byte[] SOLOM_CP =
    		{
    		    0x45, (byte) 0xC3, (byte) 0xB9, 0x77, 0x4A, 0x7A, 0x46, 0x7C,
    		    (byte) 0xBB, (byte) 0xB9, 0x2A, 0x63, 0x12, 0x6B, 0x3B, (byte) 0xA2
    		};

    		public static final byte[] SOLOM_HISTORY_CP =
    		{
    		    (byte) 0xE6, 0x7D, 0x50, 0x7E, 0x7C, (byte) 0xF8, 0x45, (byte) 0x87,
    		    (byte) 0xA5, 0x26, 0x0C, 0x2E, (byte) 0xC6, 0x10, (byte) 0xE0, (byte) 0xB4
    		};

    		public static final byte[] SOLOM_HISTORY_DATA =
    		{
    		    (byte) 0xF8, (byte) 0xD9, 0x74, (byte) 0x91, (byte) 0x93, (byte) 0xE1, 0x43, (byte) 0xA1,
    		    (byte) 0xB2, (byte) 0x9D, (byte) 0xE8, (byte) 0xB5, (byte) 0x90, (byte) 0xAD, (byte) 0xB9, (byte) 0x9F
    		};

    		public static final byte[] KEY_EXCHANGE_CP =
    		{
    		    (byte) 0xBA, 0x14, (byte) 0xCF, 0x70, 0x1E, 0x15, 0x11, (byte) 0xE4,
    		    (byte) 0x8C, 0x21, 0x08, 0x00, 0x20, 0x0C, (byte) 0x9A, 0x66
    		};
    }
    
    
	/**
	 * Returns UUID16 according to list which the op code belongs to.
	 * @param OpCode [in]
	 *       Range: ControlPointConstant.OpCode.xxxx
	 *       Unit: int
	 *       Scaling 1
	 *       
	 * @return uuid16 [out]
	 *       Range: BlueConstant.UUID16.xxxx
	 *       Unit: int
	 *       Scaling 1  
	 */
	public static int getUUID16(int opCode)
	{
		if((ControlPointConstant.OpGroup.IDS_READ.contains(opCode)))
		{
			return UUID16.IDD_STATUS_READER_CP;
		}
		else if(ControlPointConstant.OpGroup.IDS_CMD.contains(opCode))
		{
			return UUID16.IDD_COMMAND_CP;
		}
		else
		{
			return UUID16.BLANK;	
		}
		
	}
	/**
	 * Returns UUID128 byte array according to list which the op code belongs to.
	 * @param OpCode [in]
	 *       Range: ControlPointConstant.OpCode.xxxx
	 *       Unit: int
	 *       Scaling 1
	 *       
	 * @return uuid16 [out]
	 *       Range: BlueConstant.UUID128.xxxx
	 *       Unit: byte[]
	 *       Scaling: 1  
	 */
	public static byte[] getUUID128(int opCode) 
	{
		if((ControlPointConstant.OpGroup.SOLOM.contains(opCode)))
		{
			return UUID128.SOLOM_CP;
		}
		else if(ControlPointConstant.OpGroup.SOLOM_HISTORY.contains(opCode))
		{
			return UUID128.SOLOM_HISTORY_CP;
		}
		else if(ControlPointConstant.OpGroup.KEY_EXCHANGE.contains(opCode))
		{
			return UUID128.KEY_EXCHANGE_CP;
		}
		else
		{
			return UUID128.BLANK;	
		}
	}
}

/*
 * ===========================================================================
 *
 * Revision history
 *  
 * ===========================================================================
 */
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
