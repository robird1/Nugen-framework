package com.accu_chek.solo_m.rcapp.application.solompumpservice;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.util.Log;

import com.accu_chek.solo_m.rcapp.application.basal.comms.WriteBrpTemplate;
import com.accu_chek.solo_m.rcapp.application.basal.comms.database.BdData;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLERequestParameter;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SFloat;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

public class BasalBRP {

    // ArrayList of BRP Data SafetyByteArray
    private ArrayList<SafetyByteArray> mBrpDataArray = null;

    // Context of ACT71
    private Context mACT71RhContext = null;
    
    // Time Block Number in Each Data Array
    private static final byte TB_IN_ARRAY = 3;

    // SafetyBoolean Byte Value
    private static final byte SAFETY_TRUE = SafetyBoolean.TRUE.getByte();

    // Time Block Array Basic Length
    private static final int TB_ARRAY_BASIC_LEN = 9;
    
    // One Time Block Data Length
    private static final int TB_DATA_LEN = 4;
    
    // Bit Operation
    private static final byte BIT0_TRUE = (byte)0x01;

    private static final byte BIT1_TRUE = (byte)0x02;    
    
    private static final byte BIT2_TRUE = (byte)0x04;
    
    // BRP Template Number
    private static final byte BRP_TEMPLATE_NUMBER = (byte)0x01; 

    // SFLOAT Byte Number
    private static final int SFLOAT_BYTE = 2;     

    // Decimal Places Change Amount
    private static final int DECIMAL_PLACES_CHANGE_AMOUNT = 500;

    // Decimal Places
    private static final int DECIMAL_PLACE_1 = 1;
    
    private static final int DECIMAL_PLACE_2 = 2;


    public BasalBRP( Context c ){
    	mACT71RhContext = c;
    }
    
    public String getHex( byte [] raw ) {
    	String HEXES = "0123456789ABCDEF";
        if ( raw == null ) {
            return null;
        }
        final StringBuilder hex = new StringBuilder( 2 * raw.length );
        for ( final byte b : raw ) {
            hex.append(HEXES.charAt((b & 0xF0) >> 4))
                .append(HEXES.charAt((b & 0x0F)));
        }
        return hex.toString();
    }
    
    public List<String> getBrp(){
Log.i("kayjean","getBrp 1");    	
        int mTotalArrayNum = mBrpDataArray.size();
Log.i("kayjean","getBrp mTotalArrayNum " + mTotalArrayNum );        
        List ret = new ArrayList();
Log.i("kayjean","getBrp 3");
		for( int i = 0 ; i < mTotalArrayNum ; i++ ){
			Log.i("kayjean","getBrp 4");
			ret.add( getHex(mBrpDataArray.get(i).getByteArray()) );
			Log.i("kayjean","getBrp 5");
		}
		return ret;
    }
    
    /**
     * Create BRP data ArrayList for BLE communication
     *   
     * @return None
     * 
     */
    
    protected void createBrpDataArray()
    {
        // Data Base 
        BdData bdData = new BdData();
        
        // Array Flag
        SafetyBoolean isEnd = SafetyBoolean.FALSE;
        SafetyBoolean is2ndBlockExist = SafetyBoolean.FALSE;
        SafetyBoolean is3rdBlockExist = SafetyBoolean.FALSE;
        
        // Time Block Array
        SafetyByteArray arrayTb = null;
        
        // Number of Time block data arrays
        int iTbArrayNum = 0;
        
        // Time Block Number in Last Array
        int iLastTbArrayNum = 0;
        
        
        // Create ArrayList
        mBrpDataArray = new ArrayList<SafetyByteArray>();
        
        // Clear ArrayList
        mBrpDataArray.clear();
        
        // Load BRP from database
        bdData.loadBrpFromDatabase(mACT71RhContext);
        
        // Calculate Last TB array number
        iLastTbArrayNum = bdData.getNumberOfTimeBlocks() % TB_IN_ARRAY;
        
        // Calculate TB array number
        if (iLastTbArrayNum == 0)
        {
            iTbArrayNum = (int)(bdData.getNumberOfTimeBlocks() / TB_IN_ARRAY);
        }
        else
        {
            iTbArrayNum = 
                    (int)(bdData.getNumberOfTimeBlocks() / TB_IN_ARRAY) + 1;
        }
        
        // 1st Time Block Number
        byte bTb1stNumber = 0;
        
        // Time Block Number (From 0)
        int iTbNum = 0;
        
        // 1st Time Block Duration
        int iTb1stDuration = 0;
        SafetyChannel<Integer> sTb1stDuration = null;
        
        // Time Block End Time
        int iEndTime1 = 0;
        int iEndTime2 = 0;
        
        // 1st Time Block Amount 
        SafetyChannel<Integer> sTb1stAmount = null;
        
        // 2nd Time Block Duration
        int iTb2ndDuration = 0;
        SafetyChannel<Integer> sTb2ndDuration = null;

        // 2nd Time Block Amount
        SafetyChannel<Integer> sTb2ndAmount = null;
  
        // 3rd Time Block Duration
        int iTb3rdDuration = 0;
        SafetyChannel<Integer> sTb3rdDuration = null;
        
        // 3rd Time Block Amount
        SafetyChannel<Integer> sTb3rdAmount = null;
        
        // Flag
        byte isFlag = 0;
        
        
        // Make Time Block Arrays
        for (int i=1; i<=iTbArrayNum; i++)
        {
            // Determine Flag
            // Last Array
            if (i == iTbArrayNum)
            {
                isEnd = SafetyBoolean.TRUE;
                
                // Block Exists Flag
                switch (iLastTbArrayNum)
                {
                case 0:
                    
                    is2ndBlockExist = SafetyBoolean.TRUE;
                    is3rdBlockExist = SafetyBoolean.TRUE;
                    
                    break;
                case 1:
                    
                    is2ndBlockExist = SafetyBoolean.FALSE;
                    is3rdBlockExist = SafetyBoolean.FALSE;                    
                    
                    break;
                case 2:
                    
                    is2ndBlockExist = SafetyBoolean.TRUE;
                    is3rdBlockExist = SafetyBoolean.FALSE;                    
                    
                    break;
                default:
                    
                    is2ndBlockExist = SafetyBoolean.FALSE;
                    is3rdBlockExist = SafetyBoolean.FALSE;                    
                    
                    break;
                }
            }
            else
            {
                isEnd = SafetyBoolean.FALSE;
                
                is2ndBlockExist = SafetyBoolean.TRUE;
                is3rdBlockExist = SafetyBoolean.TRUE;                
            }
            
            // 1st time block number
            bTb1stNumber = (byte)((i - 1) * TB_IN_ARRAY + 1);
            
            // 1st Time Block Duration and Amount
            iTbNum = (i - 1) * TB_IN_ARRAY;
            
            if (iTbNum == 0)
            {
                iEndTime1 = 0;
                iEndTime2 = CommonUtils.getOriginValue(
                         bdData.getTimeBlock(iTbNum).getEndTime().getValueCH1(),
                         bdData.getTimeBlock(iTbNum).getEndTime().getValueCH2());
            }
            else
            {
                iEndTime1 = CommonUtils.getOriginValue(
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH1(),
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH2());
                
                iEndTime2 = CommonUtils.getOriginValue(
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH1(),
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH2());
            }
            
            iTb1stDuration = iEndTime2 - iEndTime1;
            sTb1stDuration = CommonUtils.getSafetyChannel(iTb1stDuration); 
            
            sTb1stAmount = bdData.getTimeBlock(iTbNum).getBasal();
            
            // 2nd Time Block Duration and Amount
            iTbNum = (i - 1) * TB_IN_ARRAY + 1;
            
            isFlag = is2ndBlockExist.getByte(); 
            
            if (isFlag == SAFETY_TRUE)
            {
                iEndTime1 = CommonUtils.getOriginValue(
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH1(),
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH2());
                
                iEndTime2 = CommonUtils.getOriginValue(
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH1(),
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH2());
                
                iTb2ndDuration = iEndTime2 - iEndTime1;
                sTb2ndDuration = CommonUtils.getSafetyChannel(iTb2ndDuration); 
                
                sTb2ndAmount = bdData.getTimeBlock(iTbNum).getBasal();
            }
            else
            {
                iTb2ndDuration = 0;
                sTb2ndDuration = null; 
                
                sTb2ndAmount = null;
            }
            
            // 3rd Time Block Duration and Amount
            iTbNum = (i - 1) * TB_IN_ARRAY + 2;
            
            isFlag = is3rdBlockExist.getByte(); 
            
            if (isFlag == SAFETY_TRUE)
            {
                iEndTime1 = CommonUtils.getOriginValue(
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH1(),
                     bdData.getTimeBlock(iTbNum - 1).getEndTime().getValueCH2());
                
                iEndTime2 = CommonUtils.getOriginValue(
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH1(),
                        bdData.getTimeBlock(iTbNum).getEndTime().getValueCH2());
                
                iTb3rdDuration = iEndTime2 - iEndTime1;
                sTb3rdDuration = CommonUtils.getSafetyChannel(iTb3rdDuration); 
                
                sTb3rdAmount = bdData.getTimeBlock(iTbNum).getBasal();
            }
            else
            {
                iTb3rdDuration = 0;
                sTb3rdDuration = null; 
                
                sTb3rdAmount = null;
            }            
            
            // Create time block array
            arrayTb = createBrpTbArray(isEnd,
                                       is2ndBlockExist,
                                       is3rdBlockExist,
                                       bTb1stNumber,
                                       sTb1stDuration,
                                       sTb1stAmount,
                                       sTb2ndDuration,
                                       sTb2ndAmount,
                                       sTb3rdDuration,
                                       sTb3rdAmount);
            
            // Set time block array into ArrayList
            mBrpDataArray.add(arrayTb);
        }
    }    
    
    /**
     * Create BRP time block array for BLE communication
     * 
     * @param isEnd [in] SafetyBoolean
     * 
     *         Flag of end of package
     *         
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     *         
     * @param is2ndBlockExist [in] SafetyBoolean
     * 
     *         Flag of 2nd time block existing
     *         
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     *         
     * @param is3rdBlockExist [in] SafetyBoolean
     * 
     *         Flag of 3rd time block existing
     *         
     *         Range: Valid SafetyBoolean object
     *         Unit: SafetyBoolean
     *         Scaling: 1
     *         
     * @param bTb1stNumber [in] int
     * 
     *         1st time block number in this array
     *         
     *         Range: 1 to 24
     *         Unit: int
     *         Scaling: 1
     *         
     * @param sTb1stDuration [in] SafetyChannel<Integer>
     * 
     *         Duration of 1st time block in this array
     *         
     *         Range: Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @param sTb1stAmount [in] SafetyChannel<Integer>
     * 
     *         Amount of 1st time block in this array
     *         
     *         Range: Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @param sTb2ndDuration [in] SafetyChannel<Integer>
     * 
     *         Duration of 2nd time block in this array
     *         
     *         Range: null, Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @param sTb2ndAmount [in] SafetyChannel<Integer>
     * 
     *         Amount of 2nd time block in this array
     *         
     *         Range: null, Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @param sTb3rdDuration [in] SafetyChannel<Integer>
     * 
     *         Duration of 3rd time block in this array
     *         
     *         Range: null, Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @param sTb3rdAmount [in] SafetyChannel<Integer>
     * 
     *         Amount of 3rd time block in this array
     *         
     *         Range: null, Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *   
     * @return SafetyByteArray [out] 
     * 
     *         Byte array sent to MP
     *         
     *         Range: Valid SafetyByteArray object
     *         Unit: SafetyByteArray
     *         Scaling: 1
     * 
     */
    
    protected SafetyByteArray createBrpTbArray(SafetyBoolean isEnd,
                                                 SafetyBoolean is2ndBlockExist,
                                                 SafetyBoolean is3rdBlockExist,
                                                 byte bTb1stNumber,
                                       SafetyChannel<Integer> sTb1stDuration,
                                       SafetyChannel<Integer> sTb1stAmount,
                                       SafetyChannel<Integer> sTb2ndDuration,
                                       SafetyChannel<Integer> sTb2ndAmount,
                                       SafetyChannel<Integer> sTb3rdDuration,
                                       SafetyChannel<Integer> sTb3rdAmount)
    {
        // byte array
        byte[] bData = null;
        ByteBuffer bbData = null; 
        
        // Array Length
        int iLen = 0;
        
        // Original Value of Duration
        int iOriDuration = 0;
        
        // Flag Field
        byte bFlag = 0;
        
        // Check Result
        byte bIsEnd = isEnd.getByte();
        byte bIs2ndBlockExist = is2ndBlockExist.getByte();
        byte bIs3rdBlockExist = is3rdBlockExist.getByte();

        
        // Create Byte Array
        iLen = TB_ARRAY_BASIC_LEN;
        
        if (bIs2ndBlockExist == SAFETY_TRUE)
        {
            iLen = iLen + TB_DATA_LEN;
        }
        
        if (bIs3rdBlockExist == SAFETY_TRUE)
        {
            iLen = iLen + TB_DATA_LEN;
        }
        
        bData = new byte [iLen];
        bbData = ByteBuffer.wrap(bData);
        
        // Set endian and reset ByteBuffer position
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        bbData.rewind();        
                
        // Create FlagField
        // Bit0 : End Transaction
        if (bIsEnd == SAFETY_TRUE)
        {
            bFlag = BIT0_TRUE;
        }
        
        // Bit1 : Second Time Block Present
        if (bIs2ndBlockExist == SAFETY_TRUE)
        {
            bFlag = (byte) (bFlag + BIT1_TRUE);
        }
        
        // Bit2 : Third Time Block Present
        if (bIs3rdBlockExist == SAFETY_TRUE)
        {
            bFlag = (byte) (bFlag + BIT2_TRUE);
        }
        
        // Insert OpCode
        bbData.put((byte)0xCC);
        bbData.put((byte)0x0F);
        
        // Insert FlagField
        bbData.put(bFlag);
        
        // Insert BRP Template Number
        bbData.put(BRP_TEMPLATE_NUMBER);
        
        // Insert 1st Time Block Number Index
        bbData.put(bTb1stNumber);
        
        // Insert 1st Time Block Duration
        iOriDuration = CommonUtils.getOriginValue(sTb1stDuration.getValueCH1(),
                                                  sTb1stDuration.getValueCH2());
        
        bbData.putShort((short)iOriDuration);
        
        // Insert 1st Time Block Amount
        bbData.put(safetyIntToSFloatByteArray(sTb1stAmount));
        
        // Insert 2nd Time Block Duration
        if (bIs2ndBlockExist == SAFETY_TRUE)
        {
            iOriDuration = CommonUtils.getOriginValue(
                                                sTb2ndDuration.getValueCH1(),
                                                sTb2ndDuration.getValueCH2());            
            
            bbData.putShort((short)iOriDuration);
        
            // Insert 2nd Time Block Amount
            bbData.put(safetyIntToSFloatByteArray(sTb2ndAmount));
        }
        
        // Insert 3rd Time Block Duration
        if (bIs3rdBlockExist == SAFETY_TRUE)
        {
            iOriDuration = CommonUtils.getOriginValue(
                                                sTb3rdDuration.getValueCH1(),
                                                sTb3rdDuration.getValueCH2());            
            
            bbData.putShort((short)iOriDuration);
            
            // Insert 3rd Time Block Amount
            bbData.put(safetyIntToSFloatByteArray(sTb3rdAmount));
        }
        
        return new SafetyByteArray(bData, CRCTool.generateCRC16(bData));
    }

    /**
     * SafetyInteger to SFloat Byte Array
     * 
     * @param scData [in] SafetyChannel<Integer>
     * 
     *         Data for transition
     *         
     *         Range : Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *   
     * @return byte[] [out] 
     * 
     *         Byte array of Little-endian SFLoat value 
     *         
     *         Range : Valid byte[] object
     *         Unit: byte[]
     *         Scaling: 1
     * 
     */
    
    protected byte[] safetyIntToSFloatByteArray(SafetyChannel<Integer> scData)
    {
        // Byte Array
        byte[] bData = new byte [SFLOAT_BYTE];
        
        // Byte Buffer
        ByteBuffer bbData = ByteBuffer.allocate(4);
        
        // SFloat Value
        SFloat sfData = null;
        
        // Int Value
        int iOriData = 0;
        int iData = 0;
        
        // Make SFloat
        iOriData = CommonUtils.getOriginValue(scData.getValueCH1(), 
                                              scData.getValueCH2());
        
        if (iOriData == 0)
        {
            iData = 0x0000;
        }
        else
        {
            if (iOriData >= DECIMAL_PLACES_CHANGE_AMOUNT)
            {
                iOriData = iOriData / 10;
                sfData = new SFloat(CommonUtils.getSafetyChannel(iOriData), 
                                    DECIMAL_PLACE_1);
            }
            else
            {
                sfData = new SFloat(scData, DECIMAL_PLACE_2);
            }
            
            iData = sfData.getValue().get();
        }
        
        // Set endian and reset position 
        bbData.order(ByteOrder.LITTLE_ENDIAN);
        bbData.rewind();
        
        // Insert data to byte array
        bbData.putInt(iData);
        
        // Get First 2 bytes of byte array
        bbData.rewind();
        bbData.get(bData);
        
        return bData;
    }    
	
}
