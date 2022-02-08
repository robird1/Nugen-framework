/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.config.MeterParameter
 * Brief: Storage of Single Meter Parameter
 *
 * Create Date: 03/03/2015
 * $Revision: 23369 $
 * $Author: WilliyChiang $
 * $Id: MeterParameter.java 23369 2015-11-05 07:40:14Z WilliyChiang $
 */
package com.accu_chek.solo_m.rcapp.application.config;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;

public class MeterParameter  // Meter Parameter Content
{
    private String mStrIDName = "";
    private String mStrType = "";
    private String mStrValue = "";
    private String mStrCRC = "";
    
    /**
     * Get ID Name String
     * 
     * @param None
     * @return String [out] Value of mStrIDName.
     *          Range: Valid String
     *          Unit: String
     *          Scaling: 1         
     */        
    public String getIDName()
    {
        return mStrIDName;
    }
    
    /**
     * Get Type String
     * 
     * @param None
     * @return String [out] Value of mStrType. 
     *          Range: Valid String
     *          Unit: String
     *          Scaling: 1        
     */        
    public String getType()
    {
        return mStrType;
    }
    
    /**
     * Get Value String
     * 
     * @param None
     * @return String [out] Value of mStrValue.
     *          Range: Valid String
     *          Unit: String
     *          Scaling: 1         
     */        
    public String getValue()
    {
        return mStrValue;
    }
    
    /**
     * Get CRC String
     * 
     * @param None
     * @return String [out] Value of mStrCRC
     *          Range: Valid String
     *          Unit: String
     *          Scaling: 1    
     */        
    public String getCRC()
    {
        return mStrCRC;
    }    
    
    /**
     * Set ID Name String
     * 
     * @param strIDName [in] String for Setting into mStrIDName
     * 
     *          NullPointerException will be handled at the calling function.
     *
     *          Range: Valid String
     *          Unit: String
     *          Scaling: 1
     *          
     * @return None         
     */        
    public void setIDName(String strIDName)
    {
        this.mStrIDName = strIDName;
    }        
    
    /**
     * Set Type String
     * 
     * @param strType [in] String for Setting into mStrType
     * 
     *          NullPointerException will be handled at the calling function.
     *
     *          Range: Valid String
     *          Unit: String
     *          Scaling: 1
     *          
     * @return None         
     */        
    public void setType(String strType)
    {
        this.mStrType = strType;
    }
    
    /**
     * Set Value String
     * 
     * @param strValue [in] String for Setting into mStrValue
     * 
     *          NullPointerException will be handled at the calling function.
     *
     *          Range: Valid String
     *          Unit: String
     *          Scaling: 1
     * 
     * @return None         
     */        
    public void setValue(String strValue)
    {
        this.mStrValue = strValue;
    }
    
    /**
     * Set CRC String
     * 
     * @param strCRC [in] String for Setting into mStrCRC
     * 
     *          NullPointerException will be handled at the calling function.
     *
     *          Range: Valid String
     *          Unit: String
     *          Scaling: 1
     * 
     * @return None         
     */        
    public void setCRC(String strCRC)
    {
        this.mStrCRC = strCRC;
    }        
    
    /**
     * Compare stored CRC and CRC calculated by mStrIDName, mStrType and mStrValue
     * 
     * @return SafetyBoolean [out] Return if the process is running successful. 
     *           SafetyBoolean.TRUE -- Success 
     *           SafetyBoolean.FALSE -- Fail
     *
     *          Range: SafetyBoolean.TRUE or SafetyBoolean.FALSE
     *          Unit: SafetyBoolean
     *          Scaling: 1
     */        
    public SafetyBoolean compareCRC()
    {
        // Result of Comparison
        SafetyBoolean isSame = SafetyBoolean.FALSE;
        
        // Variable for Calculate CRC
        StringBuilder sb = new StringBuilder();
        byte[] byBuffer = null;
        int iCRCValue = 0;
        int oCRCValue = 0;
        
        // Calculate CRC
        sb.append(mStrIDName);
        sb.append(mStrType);
        sb.append(mStrValue);
        
        byBuffer = new String(sb).getBytes();
        
        iCRCValue = CRCTool.generateCRC16(byBuffer);
        
        // Check CRC16        
        oCRCValue = Integer.valueOf(mStrCRC, 10);            
        
        if (iCRCValue == oCRCValue)
        {
            isSame = SafetyBoolean.TRUE;
        }
        else
        {
            isSame = SafetyBoolean.FALSE;
        }
                
        return isSame;
    }        
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// Update comments for wording refine
// (R14734 2015-08-13 23:21:10 JacksonHuang)
// ----------------------------------------------------------------------------
// Update variable name of compareCRC
