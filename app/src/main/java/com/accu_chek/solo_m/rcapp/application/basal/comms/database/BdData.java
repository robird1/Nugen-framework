/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: BdData
 * Brief: Data processing base class of basal delivery 
 *
 * Create Date: 11/06/2015
 * $Revision: 25150 $
 * $Author: JacksonHuang $
 * $Id: BdData.java 25150 2015-11-30 09:21:10Z JacksonHuang $
 */

package com.accu_chek.solo_m.rcapp.application.basal.comms.database;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyChannel;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.BasalDataModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.data.nugendata.BasalProfileTable;
import com.accu_chek.solo_m.rcapp.data.nugendata.BasalTimeBlockTable;

import java.util.ArrayList;

public class BdData
{
    // Basic TBR ID
    public static final int BASIC_TBR_ID = 0x3A;
    
    // Result of Checking Time Blocks IU Being Equals To Zero
    public static final int ALL_TB_IU_ZERO = 0x0F;
    
    public static final int PART_TB_IU_ZERO = 0x33;
    
    public static final int NO_TB_IU_ZERO = 0x3A;  
    
    // BRP Action flag
    public static final int ADD_BRP = 0x0F;
    
    public static final int UPDATE_BRP = 0x33;
    
    // Latest Edited BRP
    public static final int LATEST_EDITED_BRP = 
                                     HammingDistance.SAFETY_NUMBER_VALUE_0015;
    
    // SafetyBoolean Byte Value
    private static final byte SAFETY_TRUE = SafetyBoolean.TRUE.getByte();
    
    private static final byte SAFETY_FALSE = SafetyBoolean.FALSE.getByte();
    
    // KEY String
    private static final String KEY_BASIC_TBR_PERCENT = "tbr_basic_percentage";
    
    private static final String KEY_BASIC_TBR_DURATION = "tbr_basic_duration";
    
    private static final String KEY_BASIC_TBR_ACTIVATE = "tbr_basic_activate";
    
    private static final String KEY_BASIC_TBR_STATUS = "tbr_basic_status";
    
    private static final String KEY_BASIC_TBR_EDIT = "tbr_basic_edit";
    
    private static final String KEY_PERCENT = "_percentage";
    
    private static final String KEY_DURATION = "_duration";
    
    private static final String KEY_ACTIVATE = "_activate";
    
    private static final String KEY_STATUS = "_status";
    
    private static final String KEY_EDIT = "_edit";
    
    private static final String KEY_CUS_TBR = "tbr_cus";
    
    // TBR Activation Status -- Enabled
    public static final int TBR_ENABLED = 
                                      HammingDistance.SAFETY_NUMBER_VALUE_0090;
        
    // TBR Activation Status -- Disabled    
    public static final int TBR_DISABLED = 
                                      HammingDistance.SAFETY_NUMBER_VALUE_0091;    
    
    // Default Value
    private static final int DEFAULT_TBR_MAX_NUMBER = 5;
    
    // TBR max number
    private int mTbrMaxNumber = DEFAULT_TBR_MAX_NUMBER;
    
    // Database access
    private DbBrp mDB = new DbBrp();
    
    private DbBrpTb mDBtb = new DbBrpTb();
    
    // BRP
    private BrpData mBrp = null;
    
    private BrpData mBrpBackup = null;
    
    // Time block array of assigned basal rate profile
    private ArrayList<BrpTbData> mBrpTbSet = null;
    
    private ArrayList<BrpTbData> mBrpTbSetBackup = null;
    
    // TBR
    private TbrData mTBR = null;
    
    private TbrData mTBRbackup = null;
    
    // Selected TBR number
    private SafetyChannel<Integer> mTbrNumberNow = null; 
    
    // Selected BRP number
    private SafetyChannel<Integer> mBrpIDNow = null;
    
    // Selected BRP Time Block Number
    private SafetyChannel<Integer> mTbIDNow = null;
    
    
    /**
     * Class Constructor
     * 
     */
    public BdData()
    {
        loadTbrMaxNumber();
    }
    
    /**
     * load max number of TBR from Config Matrix
     * 
     * @return None
     * 
     */    
    protected void loadTbrMaxNumber()
    {
        // Key String
        SafetyString ssCmKey = null;
                
        // Get BRP max number from Config Matrix
        ssCmKey = new SafetyString(ConfigParameter.KEY_TBR_MAX_COUNT,
                CRCTool.generateCRC16(ConfigParameter.KEY_TBR_MAX_COUNT.getBytes()));
        
        mTbrMaxNumber = ReadConfig.getIntegerDataByKey(ssCmKey).get();
    }
    
    /**
     * Return max number of TBR
     * 
     * @return int [out]
     * 
     *         BRP max number
     *         
     *         Range: Data from Config Matrix
     *         Unit: int
     *         Scaling: 1 
     * 
     */    
    public int getTbrMaxNumber()
    {
        return mTbrMaxNumber;
    }    

    
    /**
     * Create new mTBR
     * 
     * @return TemporaryBasalRate [out]
     * 
     *          Instance of TemporaryBasalRate
     *          
     *          Range: Valid TemporaryBasalRate 
     *          Unit: TemporaryBasalRate
     *          Scaling: 1
     * 
     */    
    public TbrData createTbr()
    {    
        if (mTBR != null)
        {
            clearTbr(mTBR);
        }
        
        mTBR = new TbrData();
        
        return mTBR;
    }
    
    /**
     * Clear TemporaryBasalRate attributes and set to null
     * 
     * @param TbrData [in]
     * 
     *          Instance of TemporaryBasalRate
     *          
     *          Range: Valid TemporaryBasalRate 
     *          Unit: TemporaryBasalRate
     *          Scaling: 1
     * 
     * @return None
     * 
     */    
    public void clearTbr(TbrData tbr)
    {    
        if (tbr != null)
        {
            tbr.setPercentage(null);
            tbr.setDuration(null);
        }
        
        tbr = null;
    }    
    
    /**
     * Create new mTBRbackup
     * 
     * @return TemporaryBasalRate [out]
     * 
     *          Instance of TemporaryBasalRate
     *          
     *          Range: Valid TemporaryBasalRate 
     *          Unit: TemporaryBasalRate
     *          Scaling: 1
     * 
     */    
    public TbrData createTbrBackup()
    {    
        if (mTBRbackup != null)
        {
            clearTbr(mTBRbackup);
        }
        
        mTBRbackup = new TbrData();
        
        return mTBRbackup;
    }
    
    /**
     * Get mTBR
     * 
     * @return TemporaryBasalRate [out]
     * 
     *          Instance of mTBR
     *          
     *          Range: Valid TemporaryBasalRate 
     *          Unit: TemporaryBasalRate
     *          Scaling: 1
     * 
     */    
    public TbrData getTbr()
    {    
        if (mTBR == null)
        {
            callEmwr(EMWRList.EMW45901);
        }
        else
        {
            // Apply to the coding standard 
        }
        
        return mTBR;
    }
    
    /**
     * Get mTBRbackup
     * 
     * @return TemporaryBasalRate [out]
     * 
     *          Instance of mTBRbackup
     *          
     *          Range: Valid TemporaryBasalRate 
     *          Unit: TemporaryBasalRate
     *          Scaling: 1
     * 
     */    
    public TbrData getTbrBackup()
    {    
        if (mTBRbackup == null)
        {
            callEmwr(EMWRList.EMW45902);
        }
        else
        {
            // Apply to the coding standard 
        }
        
        return mTBRbackup;
    }    
    
    /**
     * Create new mBrp
     * 
     * @return BasalRateProfile [out]
     * 
     *          Instance of BasalRateProfile
     *          
     *          Range: Valid BasalRateProfile 
     *          Unit: BasalRateProfile
     *          Scaling: 1
     * 
     */    
    public BrpData createBrp()
    {    
        if (mBrp != null)
        {
            clearBrp(mBrp);
        }
        
        mBrp = new BrpData();
        
        return mBrp;
    }
    
    /**
     * Clear BasalRateProfile attributes and set to null
     * 
     * @param brp [in] BasalRateProfile
     * 
     *          Instance of BasalRateProfile
     *          
     *          Range: Valid BasalRateProfile 
     *          Unit: BasalRateProfile
     *          Scaling: 1
     * 
     * @return None
     * 
     */    
    public void clearBrp(BrpData brp)
    {    
        if (brp != null)
        {
            brp.setProfileKeyID(null);
            brp.setProfileName(null);
            brp.setTotalBasal(null);
            brp.setActivated(null);
            brp.setOrderingInfo(null);
        }
        
        brp = null;
    }    
    
    /**
     * Create new mBrpBackup
     * 
     * @return BasalRateProfile [out]
     * 
     *          Instance of BasalRateProfile
     *          
     *          Range: Valid BasalRateProfile 
     *          Unit: BasalRateProfile
     *          Scaling: 1
     * 
     */    
    public BrpData createBrpBackup()
    {    
        if (mBrpBackup != null)
        {
            clearBrp(mBrpBackup);
        }
        
        mBrpBackup = new BrpData();
        
        return mBrpBackup;
    }
    
    /**
     * Create new time blocks set
     * 
     * @return None
     * 
     */    
    public void createTimeBlocksSet()
    {    
        if (mBrpTbSet != null)
        {
            clearTbSet(mBrpTbSet);
        }
        
        mBrpTbSet = new ArrayList<BrpTbData>();
    }
    
    /**
     * Clear time blocks set
     * 
     * @param tbSet [in] ArrayList<BrpTimeBlock>
     * 
     *          Instance of ArrayList<BrpTimeBlock>
     *          
     *          Range: Valid ArrayList<BrpTimeBlock> 
     *          Unit: ArrayList<BrpTimeBlock>
     *          Scaling: 1
     * 
     * @return None
     * 
     */    
    public void clearTbSet(ArrayList<BrpTbData> tbSet)
    {    
        // Size of TbSet
        int iSize = 0;
        
        // Clear each TB
        if (tbSet != null)
        {
            iSize = tbSet.size();
            
            for (int i = 0; i < iSize; i++)
            {
                clearTb(tbSet.get(i));
            }
            
            // Clear data set
            tbSet.clear();
            
            // Set to null
            tbSet = null;
        }
        else
        {
            // Apply to coding standard
        }
    }
    
    /**
     * Clear time block
     * 
     * @param tb [in] BrpTimeBlock
     * 
     *          Instance of time block
     *          
     *          Range: Valid BrpTimeBlock object 
     *          Unit: BrpTimeBlock
     *          Scaling: 1
     * 
     * @return None
     * 
     */    
    public void clearTb(BrpTbData tb)
    {    
        if (tb != null)
        {
            tb.setProfileKeyID(null);
            tb.setBasal(null);
            tb.setEndTime(null);
        }
        else
        {
            // Apply to coding standard
        }
    }
    
    /**
     * Create new time blocks set backup
     * 
     * @return None
     * 
     */    
    public void createTimeBlocksSetBackup()
    {    
        if (mBrpTbSetBackup != null)
        {
            clearTbSet(mBrpTbSetBackup);
        }
        
        mBrpTbSetBackup = new ArrayList<BrpTbData>();
    }
    
    /**
     * Get mBrp
     * 
     * @return BasalRateProfile [out]
     * 
     *          Instance of mBrp
     *          
     *          Range: Valid BasalRateProfile 
     *          Unit: BasalRateProfile
     *          Scaling: 1
     * 
     */    
    public BrpData getBrp()
    {    
        if (mBrp == null)
        {
            callEmwr(EMWRList.EMW45903);
        }
        else
        {
            // Apply to the coding standard 
        }
        
        return mBrp;
    }
    
    /**
     * Get mBrpBackup
     * 
     * @return BasalRateProfile [out]
     * 
     *          Instance of mBrpBackup
     *          
     *          Range: Valid BasalRateProfile 
     *          Unit: BasalRateProfile
     *          Scaling: 1
     * 
     */    
    public BrpData getBrpBackup()
    {    
        if (mBrpBackup == null)
        {
            callEmwr(EMWRList.EMW45904);
        }
        else
        {
            // Apply to the coding standard 
        }
        
        return mBrpBackup;
    }        
            
    /**
     * Set selected TBR number
     * 
     * @param TbrNumberNow [in] SafetyChannel<Integer>
     * 
     *         Selected TBR number 
     *         
     *         Range: Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @return None
     * 
     */    
    public void setTbrNumberNow(SafetyChannel<Integer> TbrNumberNow)
    {
        this.mTbrNumberNow = TbrNumberNow;
    }    
    
    /**
     * Return selected TBR number
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *         Selected TBR number
     *         
     *         Range: Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1 
     * 
     */    
    public SafetyChannel<Integer> getTbrNumberNow()
    {
        if (mTbrNumberNow == null)
        {
            callEmwr(EMWRList.EMW45905);
        }
        else
        {
            // Apply to coding standard
        }
        
        return mTbrNumberNow;
    }
    
    /**
     * Set selected BRP id
     * 
     * @param BrpIDNow [in] SafetyChannel<Integer>
     * 
     *         Selected BRP id 
     *         
     *         Range: Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     *         
     * @return None
     * 
     */    
    public void setBrpIDNow(SafetyChannel<Integer> BrpIDNow)
    {
        this.mBrpIDNow = BrpIDNow;
    }    
    
    /**
     * Return selected BRP id
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *         Selected BRP id
     *         
     *         Range: Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1 
     * 
     */    
    public SafetyChannel<Integer> getBrpIDNow()
    {
        if (mBrpIDNow == null)
        {
            callEmwr(EMWRList.EMW45906);
        }
        
        return mBrpIDNow;
    }    
    
    /**
     * Set selected BRP time block id
     * 
     * @param TbIDNow [in] SafetyChannel<Integer>
     * 
     *         Selected BRP time block ID 
     *         
     *         Range: Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1 
     *         
     * @return None
     * 
     */    
    public void setTbIDNow(SafetyChannel<Integer> TbIDNow)
    {
        this.mTbIDNow = TbIDNow;
    }    
    
    /**
     * Return selected BRP time block ID
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *         Selected BRP id
     *         
     *         Range: Valid SafetyChannel<Integer> object
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1 
     * 
     */    
    public SafetyChannel<Integer> getTbIDNow()
    {
        if (mTbIDNow == null)
        {
            callEmwr(EMWRList.EMW45938);
        }
        
        return mTbIDNow;
    }    
    
    /**
     * Get TBR percentage
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param strKey [in] String
     * 
     *         TBR index
     *         
     *         Range: Valid String
     *         Unit: String
     *         Scaling: 1
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          TBR percentage
     *          
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     */    
    protected SafetyChannel<Integer> getTbrPercentage(Context context, 
                                                      String strKey)
    {
        // TBR percentage
        SafetyChannel<Integer> scPercentage = null;
        
        // Get percentage
        scPercentage = BasalDataModel.getInt(context, strKey);
        
        return scPercentage;
    }   
    
    /**
     * Get TBR duration
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param strKey [in] String
     * 
     *         TBR index
     *         
     *         Range: Valid String
     *         Unit: String
     *         Scaling: 1
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          TBR duration
     *          
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     */    
    protected SafetyChannel<Integer> getTbrDuration(Context context, 
                                                    String strKey)
    {
        // TBR duration
        SafetyChannel<Integer> scDuration = null;
        
        // Get percentage
        scDuration = BasalDataModel.getInt(context, strKey);
        
        return scDuration;
    } 
    
    /**
     * Get TBR activation time stamp
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param strKey [in] String
     * 
     *         TBR index
     *         
     *         Range: Valid String
     *         Unit: String
     *         Scaling: 1
     * 
     * @return SafetyChannel<Long> [out]
     * 
     *          TBR activation timestamp
     *          
     *          Range: Valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * 
     */    
    protected SafetyChannel<Long> getTbrActivationTimestamp(Context context, 
                                                            String strKey)
    {
        // TBR activation time stamp
        SafetyChannel<Long> scTimestamp = null;
                
        // Get status
        scTimestamp = BasalDataModel.getLong(context, strKey);
        
        return scTimestamp;
    }
    
    /**
     * Get TBR activation status
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param strKey [in] String
     * 
     *         TBR index
     *         
     *         Range: Valid String
     *         Unit: String
     *         Scaling: 1
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          TBR activation status
     *          
     *          Range: TBR_ENABLED or TBR_DISABLED
     *                 TBR_ENABLED -- TBR is activated
     *                 TBR_DISABLED -- TBR is deactivated
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     */    
    protected SafetyChannel<Integer> getTbrStatus(Context context, String strKey)
    {        
        // TBR Activation Status
        SafetyChannel<Integer> scStatus = null;
        
        // Get percentage
        scStatus = BasalDataModel.getInt(context, strKey);
        
        return scStatus;
    }
    
    /**
     * Get TBR if being latest edited
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param strKey [in] String
     * 
     *         TBR index
     *         
     *         Range: Valid String
     *         Unit: String
     *         Scaling: 1
     * 
     * @return SafetyBoolean [out]
     * 
     *          Latest-edit status
     *          
     *          Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *                 SafetyBoolean.TRUE -- Latest edited TBR
     *                 SafetyBoolean.FALSE -- Not latest edited TBR
     *          Unit: SafetyBoolean
     *          Scaling: 1
     * 
     */    
    protected SafetyBoolean getTbrLatestEdit(Context context, 
                                                       String strKey)
    {
        // TBR Latest-edit Status
        SafetyBoolean scLatestEdit = null;
        
        // Default Status
        SafetyBoolean isNotLatestEdited = SafetyBoolean.FALSE; 
                
        // Get percentage
        scLatestEdit = BasalDataModel.getSafetyBoolean(context, strKey,
                                                       isNotLatestEdited);
        
        return scLatestEdit;
    }    
    
    /**
     * Get custom TBR percentage
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param index [in] SafetyChannel<Integer>
     * 
     *         Custom TBR index
     *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          Custom TBR percentage
     *          
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     */    
    public SafetyChannel<Integer> getCustomTbrPercentage(Context context,
                                                SafetyChannel<Integer> index)
    {
        // Key String
        StringBuilder strKey = new StringBuilder();;
        // Index original value
        int iIndex = CommonUtils.getOriginValue(index.getValueCH1(), 
                                                index.getValueCH2());
        
        // Create Key
        strKey.append(KEY_CUS_TBR);
        strKey.append(iIndex);
        strKey.append(KEY_PERCENT);
        
        return getTbrPercentage(context, strKey.toString());
    }   
    
    /**
     * Get custom TBR duration
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param index [in] SafetyChannel<Integer>
     * 
     *         Custom TBR index
     *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          Custom TBR duration
     *          
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     */    
    public SafetyChannel<Integer> getCustomTbrDuration(Context context, 
                                                SafetyChannel<Integer> index)
    {
        // Key String
        StringBuilder strKey = new StringBuilder();
        // Index original value
        int iIndex = CommonUtils.getOriginValue(index.getValueCH1(), 
                                                index.getValueCH2());

        // Create Key
        strKey.append(KEY_CUS_TBR); 
        strKey.append(iIndex);
        strKey.append(KEY_DURATION);
                
        return getTbrDuration(context, strKey.toString());
    }
    
    /**
     * Get custom TBR status
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param index [in] SafetyChannel<Integer>
     * 
     *         Custom TBR index
     *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     * 
     * @return SafetyChannel<Long> [out]
     * 
     *          Custom TBR activation timestamp
     *          
     *          Range: Valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * 
     */    
    public SafetyChannel<Long> getCustomTbrActivationTimestamp(Context context,
                                                  SafetyChannel<Integer> index)
    {
        // Key String
        StringBuilder strKey = new StringBuilder();
        // Index original value
        int iIndex = CommonUtils.getOriginValue(index.getValueCH1(), 
                                                index.getValueCH2());        

        // Create Key
        strKey.append(KEY_CUS_TBR);
        strKey.append(iIndex);
        strKey.append(KEY_ACTIVATE);
                
        return getTbrActivationTimestamp(context, strKey.toString());
    }    
    
    /**
     * Get custom TBR activation status
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param index [in] SafetyChannel<Integer>
     * 
     *         Custom TBR index
     *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          TBR activation status
     *          
     *          Range: TBR_ENABLED or TBR_DISABLED
     *                 TBR_ENABLED -- TBR is activated
     *                 TBR_DISABLED -- TBR is deactivated
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     */    
    public SafetyChannel<Integer> getCustomTbrStatus(Context context,
                                    SafetyChannel<Integer> index)
    {        
        // Key String
        StringBuilder strKey = new StringBuilder();
        // Index original value
        int iIndex = CommonUtils.getOriginValue(index.getValueCH1(), 
                                                index.getValueCH2());

        // Create Key
        strKey.append(KEY_CUS_TBR);
        strKey.append(iIndex);
        strKey.append(KEY_STATUS);
                
        return getTbrStatus(context, strKey.toString());
    } 
    
    /**
     * Get custom TBR latest-edit status
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param index [in] SafetyChannel<Integer>
     * 
     *         Custom TBR index
     *         
     *         Range: Valid SafetyChannel<Integer>
     *         Unit: SafetyChannel<Integer>
     *         Scaling: 1
     * 
     * @return SafetyBoolean [out]
     * 
     *          Latest-edit status
     *          
     *          Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *                 SafetyBoolean.TRUE -- Latest edited TBR
     *                 SafetyBoolean.FALSE -- Not latest edited TBR
     *          Unit: SafetyBoolean
     *          Scaling: 1
     * 
     */    
    public SafetyBoolean getCustomTbrLatestEdit(Context context,
                                    SafetyChannel<Integer> index)
    {        
        // Key String
        StringBuilder strKey = new StringBuilder();
        // Index original value
        int iIndex = CommonUtils.getOriginValue(index.getValueCH1(), 
                                                index.getValueCH2());

        // Create Key
        strKey.append(KEY_CUS_TBR);
        strKey.append(iIndex);
        strKey.append(KEY_EDIT);
                
        return getTbrLatestEdit(context, strKey.toString());
    }    
    
    /**
     * Get basic TBR percentage
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          Basic TBR percentage
     *          
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     */    
    public SafetyChannel<Integer> getBasicTbrPercentage(Context context)
    {        
        return getTbrPercentage(context, KEY_BASIC_TBR_PERCENT);
    }   
    
    /**
     * Get basic TBR duration
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          Basic TBR duration
     *          
     *          Range: Valid SafetyChannel<Integer> object
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     */    
    public SafetyChannel<Integer> getBasicTbrDuration(Context context)
    {
        return getTbrDuration(context, KEY_BASIC_TBR_DURATION);
    }
    
    /**
     * Get basic TBR status
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     * 
     * @return SafetyChannel<Long> [out]
     * 
     *          Basic TBR activation timestamp
     *          
     *          Range: Valid SafetyChannel<Long> object
     *          Unit: SafetyChannel<Long>
     *          Scaling: 1
     * 
     */    
    public SafetyChannel<Long> getBasicTbrActivationTimestamp(Context context)
    {
        return getTbrActivationTimestamp(context, KEY_BASIC_TBR_ACTIVATE);
    }   
    
    /** 
     * Get basic TBR activation status
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     * 
     * @return SafetyChannel<Integer> [out]
     * 
     *          TBR activation status
     *          
     *          Range: TBR_ENABLED or TBR_DISABLED
     *                 TBR_ENABLED -- TBR is activated
     *                 TBR_DISABLED -- TBR is deactivated
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     */   
    
    public SafetyChannel<Integer> getBasicTbrStatus(Context context)
    {        
        return getTbrStatus(context,  KEY_BASIC_TBR_STATUS);
    }  
    
    /** 
     * Get basic TBR latest-edit status
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     * 
     * @return SafetyBoolean [out]
     * 
     *          Latest-edit status
     *          
     *          Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *                 SafetyBoolean.TRUE -- Latest edited TBR
     *                 SafetyBoolean.FALSE -- Not latest edited TBR
     *          Unit: SafetyBoolean
     *          Scaling: 1
     * 
     */   
    
    public SafetyBoolean getBasicTbrLatestEdit(Context context)
    {        
        return getTbrLatestEdit(context,  KEY_BASIC_TBR_EDIT);
    }     
    
    /**
     * Load BRP from database
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @return None
     */
    public void loadBrpFromDatabase(Context context)
    {
        // Query Result
        BasalProfileTable queryResult = null;
        BasalProfileTable queryResultBack = null;
        
        // Latest Edited BRP SafetyChannel Value
        SafetyChannel<Integer> scOrdering = CommonUtils
                                          .getSafetyChannel(
                                           BdData.LATEST_EDITED_BRP);
        
        // Clear previous BRP
        setBrpIDNow(null);
        clearBrp(mBrp);
        clearBrp(mBrpBackup);

        // Query
        queryResult = mDB.queryProfileOrderingInfo(context, scOrdering);
        queryResultBack = mDB.queryProfileOrderingInfo(context, scOrdering);
        
        if ((queryResult != null) && (queryResultBack != null)) 
        {
            // Check Integrity
            compareIntegerDataIntegrity(queryResult.getRecordId(), 
                                            queryResultBack.getRecordId());
            
            // Set BRP ID now
            setBrpIDNow(queryResult.getRecordId());
            
            // Load BRP
            loadBrp(context);
        }
        else
        {
            callEmwr(EMWRList.EMW45939);
        }
    }    
    
    /**
     * Load BRP with selected BRP ID
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *        
     * @return None
     */
    public void loadBrp(Context context)
    {
        // Query Result
        BasalProfileTable queriedBrp = null;
        BasalProfileTable queriedBrpBackup = null;       
        
        // Check BRP Number is not null
        CommonUtils.objectCheck(mBrpIDNow);
        
        // Query with BRP number
        queriedBrp = mDB.queryProfile(context, mBrpIDNow);
        queriedBrpBackup = mDB.queryProfile(context, mBrpIDNow);
        
        // Check query result is not null
        CommonUtils.objectCheck(queriedBrp);
        CommonUtils.objectCheck(queriedBrpBackup);
        
        // Insert data into BRP
        mBrp = bptToBrp(queriedBrp);
        
        // Insert data into BRP Backup
        mBrpBackup = bptToBrp(queriedBrpBackup);
        
        // Check data integrity of BRP
        compareIntegerDataIntegrity(mBrp.getProfileKeyID(), mBrpIDNow);
        
        compareIntegerDataIntegrity(mBrpBackup.getProfileKeyID(), mBrpIDNow);
        
        checkBrpDataIntegrity(mBrp, mBrpBackup);
        
        // Create new time blocks & time blocks backup
        createTimeBlocksSet();
        createTimeBlocksSetBackup();
        
        // Load BRP time blocks
        loadTimeBlocks(context, mBrpTbSet);
        
        loadTimeBlocks(context, mBrpTbSetBackup);
        
        // Check data integrity of time blocks
        checkTimeBlocksDataIntegrity();
    }
    

    
    /** 
     * Load time blocks from database
     * 
     * @param context [in] Context
     * 
     *         Context of the activity
     *         
     *         Range: Valid Context object
     *         Unit: Context
     *         Scaling: 1
     *         
     * @param BrpTbSet [in] ArrayList<BrpTimeBlock>
     * 
     *        ArrayList of time blocks
     *        
     *        Range: Valid ArrayList<BrpTimeBlock> object
     *        Unit: ArrayList<BrpTimeBlock>
     *        Scaling: 1
     *        
     * @return None
     */
    public void loadTimeBlocks(Context context, 
                               ArrayList<BrpTbData> brpTbSet)
    {        
        // BRP TimeBlock object
        BrpTbData brpTB = null;        
        // Safety EndTime object
        SafetyChannel<Integer> scEndTime = null;
        // Time blocks saved in database
        ArrayList<BasalTimeBlockTable> tbSet = null;
        int tbSetSize = 0;
        // Time 2 Channels value
        long lCH1 = 0;
        long lCH2 = 0;
        
        // Clear Data Set
        brpTbSet.clear();
            
        // Check BRP is not null
        if (mBrp == null)
        {
            callEmwr(EMWRList.EMW45903);
        }
        else
        {
            // Apply to coding standard
        }   
        
        // Check Profile ID is correct
        compareIntegerDataIntegrity(mBrpIDNow, mBrp.getProfileKeyID()); 
        
        // Get time blocks from database
        tbSet = mDBtb.queryTimeBlock(context, mBrpIDNow);
        
        if (tbSet == null)
        {
            callEmwr(EMWRList.EMW45913);
        }
        else
        {
            // Apply to coding standard
        }
        
        tbSetSize = tbSet.size();
        
        // Insert 
        for (int i = 0; i < tbSetSize; i++)
        {
            brpTB = new BrpTbData();

            lCH1 = tbSet.get(i).getEndTime().getValueCH1();
            lCH2 = tbSet.get(i).getEndTime().getValueCH2();
            scEndTime = new SafetyChannel<Integer>((int)lCH1, (int)lCH2);
            
            brpTB.setProfileKeyID(tbSet.get(i).getProfileId());
            brpTB.setEndTime(scEndTime);
            brpTB.setBasal(tbSet.get(i).getBasalRate());
            
            brpTbSet.add(brpTB);
        }
    }
    
    /**
     * Check data integrity of BRP
     * 
     * @param brp [in] BasalRateProfile
     * 
     *         BasalRateProfile for checking integrity
     *         
     *         Range: Valid BasalRateProfile object
     *         Unit: BasalRateProfile
     *         Scaling: 1
     *         
     * @param brpBack [in] BasalRateProfile
     * 
     *        BasalRateProfile Backup for checking integrity 
     *        
     *        Range: Valid BasalRateProfile object
     *        Unit: BasalRateProfile
     *        Scaling: 1  
     *        
     * @return None
     */
    public void checkBrpDataIntegrity(BrpData brp,
                                      BrpData brpBack)
    {
        compareIntegerDataIntegrity(brp.getProfileKeyID(),
                                    brpBack.getProfileKeyID());
        
        compareStringDataIntegrity(brp.getProfileName(),
                                   brpBack.getProfileName());
        
        compareIntegerDataIntegrity(brp.getTotalBasal(),
                                    brpBack.getTotalBasal());
        
        compareBooleanDataIntegrity(brp.getActivated(),
                                    brpBack.getActivated());
        
        compareIntegerDataIntegrity(mBrp.getOrderingInfo(),
                                    brpBack.getOrderingInfo());
    }
    
    /**
     * Check data integrity of time blocks
     *        
     * @return None
     */
    public void checkTimeBlocksDataIntegrity()
    {
        // Number of Time Blocks
        int iNum = 0;
        // Time Block
        BrpTbData tb = null;
        BrpTbData tbBackup = null;
        
        // Get number of time blocks
        iNum = getNumberOfTimeBlocks();
        
        // Check data integrity
        for (int i = 0; i < iNum; i++)
        {
            tb = mBrpTbSet.get(i);
            tbBackup = mBrpTbSetBackup.get(i);
            
            checkTbDataIntegrity(tb, tbBackup);
        }
    }
    
    /**
     * Check data integrity of time block
     * 
     * @param tb [in] BrpTimeBlock
     * 
     *          Time block of time block data set
     *          
     *          Range: Valid BrpTimeBlock
     *          Unit: BrpTimeBlock
     *          Scaling: 1
     *          
     * @param tbBackup [in] BrpTimeBlock
     * 
     *          Time block of time block data set backup
     *          
     *          Range: Valid BrpTimeBlock
     *          Unit: BrpTimeBlock
     *          Scaling: 1         
     *        
     * @return None
     */
    public void checkTbDataIntegrity(BrpTbData tb, BrpTbData tbBackup)
    {        
        compareIntegerDataIntegrity(tb.getProfileKeyID(), 
                                    tbBackup.getProfileKeyID());
        
        compareIntegerDataIntegrity(tb.getEndTime(), 
                                    tbBackup.getEndTime());
        
        compareIntegerDataIntegrity(tb.getBasal(), 
                                    tbBackup.getBasal());
    }    
    
    /**
     * Return number of time blocks
     *        
     * @return int [out]
     *         
     *         Number of time blocks
     *         
     *         Range: 1 to Max time block number
     *         Unit: Context
     *         Scaling: 1
     */
    public int getNumberOfTimeBlocks()
    {
        // Number of Time Blocks
        int iNum = 0;
        int iNumBackup = 0;

        
        // Get number
        iNum = mBrpTbSet.size();
        iNumBackup = mBrpTbSetBackup.size();
                
        // Check number of master set is the same as the one of backup set 
        if (iNum != iNumBackup)
        {
            callEmwr(EMWRList.EMW45912);
        }
        else
        {
            // Apply to coding standard
        }
        
        return iNum;
    }
    
    /**
     * Get time block in ArrayList data set with assigned position
     * 
     * @param index [in] int
     * 
     *        Needed time block position in ArrayList data set    
     *        
     *        Range: 0 to mBrpTbSet.size()-1
     *        Unit: int
     *        Scaling: 1 
     *        
     * @return BrpTimeBlock [out]
     * 
     *         Needed time block 
     *         
     *         Range: Valid BrpTimeBlock object
     *         Unit: BrpTimeBlock
     *         Scaling: 1 
     */ 
    public BrpTbData getTimeBlock(int index)
    {
        return mBrpTbSet.get(index);
    } 
    
    /**
     * Get backup time block in ArrayList data set with assigned position
     * 
     * @param index [in] int
     * 
     *        Needed backup time block position in ArrayList data set    
     *        
     *        Range: 0 to mBrpTbSet.size()-1
     *        Unit: int
     *        Scaling: 1 
     *        
     * @return BrpTimeBlock [out]
     * 
     *         Needed backup time block 
     *         
     *         Range: Valid BrpTimeBlock object
     *         Unit: BrpTimeBlock
     *         Scaling: 1 
     */ 
    public BrpTbData getTimeBlockBackup(int index)
    {
        return mBrpTbSetBackup.get(index);
    }    
    
    /**
     * Compare two SafetyChannel data for data integrity.
     * If the comparison fails, call EMWR
     * 
     * @param scData [in] SafetyChannel<Integer>
     * 
     *        Data for comparison  
     *        
     *        Range: Valid SafetyChannel<Integer> object
     *        Unit: SafetyChannel<Integer>
     *        Scaling: 1
     *        
     * @param scDataBak [in] SafetyChannel<Integer>
     * 
     *        Data for comparison  
     *        
     *        Range: Valid SafetyChannel<Integer> object
     *        Unit: SafetyChannel<Integer>
     *        Scaling: 1
     *                                      
     * @return None
     */
    public void compareIntegerDataIntegrity(SafetyChannel<Integer> scData,
                                            SafetyChannel<Integer> scDataBak)
    {        
        // Channel Data
        int iCH = 0;
        int iCHbak = 0;
        // Hash code
        int iHash1 = 0;
        int iHash2 = 0;  

        // Check these 2 objects are not the same one
        iHash1 = scData.hashCode();
        iHash2 = scDataBak.hashCode();
        
        if (iHash1 == iHash2)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45915);
        }
        else
        {
            // Apply to the coding standard
        }
        
        // Check two CH1 values are the same
        iCH = scData.getValueCH1();
        iCHbak = scDataBak.getValueCH1();
        
        if (iCH != iCHbak)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45918);
        }
        else
        {
            // Apply to the coding standard
        }
        
        // Check two CH2 values are the same
        iCH = scData.getValueCH2();
        iCHbak = scDataBak.getValueCH2();
        
        if (iCH != iCHbak)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45919);
        }
        else
        {
            // Apply to the coding standard
        }        
        
        // Check integrity using channel 1 of data and channel 2 of data backup   
        iCH = scData.getValueCH1();
        iCHbak = scDataBak.getValueCH2();
        
        checkIntegerDataIntegrity(iCH, iCHbak);
                
        // Check integrity using channel 2 of data and channel 1 of data backup
        iCH = scDataBak.getValueCH1();
        iCHbak = scData.getValueCH2();
        
        checkIntegerDataIntegrity(iCH, iCHbak);        
    }
    
    /**
     * Compare two SafetyChannel data for data integrity.
     * If the comparison fails, call EMWR
     * 
     * @param scData [in] SafetyChannel<Long>
     * 
     *        Data for comparison  
     *        
     *        Range: Valid SafetyChannel<Long> object
     *        Unit: SafetyChannel<Long>
     *        Scaling: 1
     *        
     * @param scDataBak [in] SafetyChannel<Long>
     * 
     *        Data for comparison  
     *        
     *        Range: Valid SafetyChannel<Long> object
     *        Unit: SafetyChannel<Long>
     *        Scaling: 1
     *                                      
     * @return None
     */
    protected void compareLongDataIntegrity(SafetyChannel<Long> scData,
                                          SafetyChannel<Long> scDataBak)
    {        
        // Channel Data
        long iCH = 0;
        long iCHbak = 0;
        // Hash code
        int iHash1 = 0;
        int iHash2 = 0;
        
        // Check these 2 objects are not the same one
        iHash1 = scData.hashCode();
        iHash2 = scDataBak.hashCode();
        
        if (iHash1 == iHash2)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45920);
        }
        else
        {
            // Apply to the coding standard
        }
        
        // Check two CH1 values are the same
        iCH = scData.getValueCH1();
        iCHbak = scDataBak.getValueCH1();
        
        if (iCH != iCHbak)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45921);
        }
        else
        {
            // Apply to the coding standard
        }
        
        // Check two CH2 values are the same
        iCH = scData.getValueCH2();
        iCHbak = scDataBak.getValueCH2();
        
        if (iCH != iCHbak)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45922);
        }
        else
        {
            // Apply to the coding standard
        } 
        
        // Check integrity using channel 1 of data and channel 2 of data backup   
        iCH = scData.getValueCH1();
        iCHbak = scDataBak.getValueCH2();
        
        checkLongDataIntegrity(iCH, iCHbak);
                
        // Check integrity using channel 2 of data and channel 1 of data backup
        iCH = scDataBak.getValueCH1();
        iCHbak = scData.getValueCH2();
        
        checkLongDataIntegrity(iCH, iCHbak);        
    }    
    
    /**
     * Compare two SafetyBoolean data for data integrity.
     * If the comparison fails, call EMWR
     * 
     * @param scData [in] SafetyBoolean
     * 
     *        Data for comparison  
     *        
     *        Range: Valid SafetyBoolean object
     *        Unit: SafetyBoolean
     *        Scaling: 1
     *        
     * @param scDataBak [in] SafetyBoolean
     * 
     *        Data for comparison  
     *        
     *        Range: Valid SafetyBoolean object
     *        Unit: SafetyBoolean
     *        Scaling: 1
     *                                      
     * @return None
     */
    protected void compareBooleanDataIntegrity(SafetyBoolean scData,
                                             SafetyBoolean scDataBak)
    {        
        // Channel Data
        byte bData = 0;
        byte bDataBak = 0;
        
        // Check byte values are the same
        bData = scData.getByte();
        bDataBak = scDataBak.getByte();
        
        if (bData != bDataBak)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45928);
        }
        else
        {
            // Apply to the coding standard
        }        
    }
    
    /**
     * Compare two SafetyString data for data integrity.
     * If the comparison fails, call EMWR
     * 
     * @param scData [in] SafetyString
     * 
     *        Data for comparison  
     *        
     *        Range: Valid SafetyString object
     *        Unit: SafetyString
     *        Scaling: 1
     *        
     * @param scDataBak [in] SafetyString
     * 
     *        Data for comparison  
     *        
     *        Range: Valid SafetyString object
     *        Unit: SafetyString
     *        Scaling: 1
     *                                      
     * @return None
     */
    public void compareStringDataIntegrity(SafetyString scData,
                                            SafetyString scDataBak)
    {        
        // String Data
        String strData = null;
        String strDataBak = null;
        // Hash code
        int iHash1 = 0;
        int iHash2 = 0;
        
        
        // CRC Data
        int iCRC = 0;
        int iCRCbak = 0;
        
        // check result
        boolean isSame = false;

        // Check these 2 objects are not the same one
        iHash1 = scData.hashCode();
        iHash2 = scDataBak.hashCode();
        if (iHash1 == iHash2)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45929);
        }
        else
        {
            // Apply to the coding standard
        }
        
        // Check two string values are the same
        strData = scData.getString();
        strDataBak = scDataBak.getString();
        
        isSame = strData.equals(strDataBak);
        
        if (isSame == false)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45930);
        }
        else
        {
            // Apply to the coding standard
        }
        
        // Check two CRC values are the same
        iCRC = scData.getCRC();
        iCRCbak = scDataBak.getCRC();
        
        if (iCRC != iCRCbak)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45931);
        }
        else
        {
            // Apply to the coding standard
        }        
    }    
    
    /**
     * Check that redundant/diverse value pair is valid.
     * 
     * If this pair is not valid, Call EMWR
     * 
     * @param iCH1 [in] int
     * 
     *          One of Redundant/diverse value pair
     * 
     *          Range: -2^31 to (2^31) -1
     *          Unit: int
     *          Scaling: 1
     *          
     * @param iCH2 [in] int
     * 
     *          The other one of Redundant/diverse value pair
     * 
     *          Range: -2^31 to (2^31) -1
     *          Unit: int
     *          Scaling: 1
     *          
     * @return None
     */    
    public void checkIntegerDataIntegrity(int iCH1, int iCH2)
    {
        byte isOK = SAFETY_FALSE;
        
        // Check if redundant/diverse value pair is not changed  
        isOK = CommonUtils.compareTwoChannel(iCH1, iCH2).getByte();

        if (isOK != SAFETY_TRUE)
        {
            // call EMWR
            callEmwr(EMWRList.EMW45916);
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
    /**
     * Check that redundant/diverse value pair is valid.
     * 
     * If this pair is not valid, call EMWR
     * 
     * @param iCH1 [in] long
     * 
     *          One of Redundant/diverse value pair
     * 
     *          Range: -2^63 to (2^63)-1
     *          Unit: long
     *          Scaling: 1
     *          
     * @param iCH2 [in] long
     * 
     *          The other one of Redundant/diverse value pair
     * 
     *          Range: -2^63 to (2^63)-1
     *          Unit: long
     *          Scaling: 1
     *          
     * @return None
     */    
    public void checkLongDataIntegrity(long iCH1, long iCH2)
    {
        byte isOK = SAFETY_FALSE;
        
        // Check if redundant/diverse value pair is not changed  
        isOK = CommonUtils.compareTwoChannel(iCH1, iCH2).getByte();

        if (isOK != SAFETY_TRUE)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45923);
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
    /**
     * Check SafetyString is valid.
     * 
     * If this string is not valid, call EMWR
     * 
     * @param strData [in] String
     * 
     *          String data
     * 
     *          Range: Valid String
     *          Unit: String
     *          Scaling: 1
     *          
     * @param iCRC [in] int
     * 
     *          CRC16 of string
     * 
     *          Range: -2^31 to (2^31)-1
     *          Unit: int
     *          Scaling: 1
     *          
     * @return None
     */    
    public void checkStringDataIntegrity(String strData, int iCRC)
    {
        // Check Result
        byte isEqual = SAFETY_FALSE;
        SafetyString ss = new SafetyString();
        
        // Check if redundant/diverse value pair is not changed  
        isEqual = ss.safetyEquals(strData, iCRC).getByte();

        if (isEqual != SAFETY_TRUE)
        {
            // Call EMWR
            callEmwr(EMWRList.EMW45917);
        }
        else
        {
            // Apply to the coding standard
        }
    }    
    
    /**
     * Transfer BasalProfileTable to BasalRateProfile
     * 
     * @param inData [in] BasalProfileTable
     * 
     *          Data from database
     * 
     *          Range: Valid BasalProfileTable
     *          Unit: BasalProfileTable
     *          Scaling: 1
     * 
     * @return BasalRateProfile [out]
     * 
     *          Data in BasalRateProfile form
     *          
     *          Range: Valid BasalRateProfile
     *          Unit: BasalRateProfile
     *          Scaling: 1
     */    
    
    public BrpData bptToBrp(BasalProfileTable inData)
    {
        // BasalRateProfile
        BrpData brp = new BrpData();
        // SafetyBoolean Value
        SafetyBoolean data = SafetyBoolean.FALSE;
        
        brp.setProfileKeyID(inData.getRecordId());
        brp.setProfileName(inData.getProfileName());
        brp.setTotalBasal(inData.getTotalBasal());
        
        data = safetyIntegerToSafetyBoolean(inData.getIsProfileActive());
        brp.setActivated(data);
        
        brp.setOrderingInfo(inData.getOrderInfo());        
        
        return brp;
    }
    
    /**
     * Transfer SafetyChannel<Integer> to SafetyBoolean
     * 
     * @param scValue [in] SafetyChannel<Integer>
     * 
     *          Data to be transferred
     * 
     *          Range: Valid SafetyChannel<Integer>
     *          Unit: SafetyChannel<Integer>
     *          Scaling: 1
     * 
     * @return SafetyBoolean [out]
     * 
     *          Transferred data
     *          
     *          Range: Valid SafetyBoolean
     *          Unit: SafetyBoolean
     *          Scaling: 1
     *          
     */    
    public SafetyBoolean safetyIntegerToSafetyBoolean(SafetyChannel<Integer> scValue)
    {
        // Transfered Result
        SafetyBoolean sbResult = SafetyBoolean.FALSE;
        // Original Value
        byte bOriValue = 0;
        
        // Get Original Integer
        bOriValue = (byte)CommonUtils.getOriginValue(scValue.getValueCH1(),
                                                     scValue.getValueCH2());
        
        // Transfer to SafetyBoolean
        if (bOriValue == SAFETY_TRUE)
        {
            sbResult = SafetyBoolean.TRUE;
        }
        else if (bOriValue == SAFETY_FALSE)
        {
            sbResult = SafetyBoolean.FALSE;
        }
        else
        {
            callEmwr(EMWRList.EMW45924);
        }
        
        return sbResult;
    }
    
    /**
     * Call EMWR for error handling
     * 
     * @param ErrorCode [in] EMWRList
     * 
     *          Internal Error Code Enum Value of Basal Delivery 
     * 
     *          Range: Valid EMWRList
     *          Unit: EMWRList
     *          Scaling: 1
     * 
     * @return None
     */
    protected void callEmwr(EMWRList ErrorCode)
    {    
        NotifyMessage msg = new NotifyMessage(ErrorCode);
     
        NotifyProxy.showEMWR(msg);
    }    
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// [New Feature] Basal Delivery comms functions
// [Update] Modify comments
// [Update] Add WaitDelivery for long time delivery time
