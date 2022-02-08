/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.BLERequestParameter
 * Brief: This class is used for setting parameter of request.
 *
 * Create Date: 2015/8/10
 * $Revision: 22298 $
 * $Author: KiddYeh $
 * $Id: BLERequestParameter.java 22298 2015-10-22 06:55:59Z KiddYeh $
 */

package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

import com.accu_chek.solo_m.rcapp.application.ble.IRequest;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

/**
 * This class is used for setting parameter of request.
 *
 */
public class BLERequestParameter
{
    
    /**
     *  Command Code of request Parameter
     */
    private int mCommandCode = -1;
    
	/**
	 *  UUID of request Parameter
	 */
    private int mUUID = -1;
    
    /**
     *  OpCode of request Parameter
     */
    private int mOpCode = -1;
    /**
     * Request general parameter
     */
    private int mParm = -1;
    
    /**
     *  Mode of request Parameter (Enable/Disable)
     */
    private SafetyBoolean mIsEnable = SafetyBoolean.FALSE;
    
    /**
     *  PinCode of request Parameter
     */
    private SafetyByteArray mPinCode = new SafetyByteArray();
    
    /**
     *  Data of request Parameter
     */
    private SafetyByteArray mData = new SafetyByteArray();
    
    /**
     *  request
     */
    private IRequest mRequest = null;
    
    /**
     * Set mode parameter in request parameter.
     * 
     * @param isEnable: the setting mode
     *            Range: A valid object of SafetyBoolean
     *            Unit: SafetyBoolean
     *            Scaling: 1
     *            
     * @return void [out]          
     *            
     */ 
   public void setIsEnable( SafetyBoolean isEnable )
   {
       mIsEnable = isEnable;
   }
   
   
   /**
    * Get mode parameter in request parameter.
    * 
    * @param N/A
    * 
    * @return isEnable: the setting mode
    *            Range: A valid object of SafetyBoolean
    *            Unit: SafetyBoolean
    *            Scaling: 1
    *            
    *            
    */ 
   public SafetyBoolean getIsEnable( )
   {
       return mIsEnable;
   }
   
   
   /**
    * Set Pin code parameter in request parameter.
    * 
    * @param pincode: the MP pin code
    *            Range: A valid object of SafetyByteArray
    *            Unit: SafetyByteArray
    *            Scaling: 1
    *            
    * @return void [out]           
    *            
    */ 
   public void setPinCode( SafetyByteArray pinCode)
   {
       mPinCode = pinCode;
   } 
   
   
   /**
    * Get Pin code parameter in request parameter.
    * 
    * @param N/A
    * 
    * @return mPinCode: the MP pin code
    *            Range: A valid object of SafetyByteArray
    *            Unit: SafetyByteArray
    *            Scaling: 1
    */ 
   public SafetyByteArray getPinCode()
   {
       return mPinCode;
   }
   
   
   /**
    * Set data parameter in request parameter.
    * 
    * @param data: 
    *            Range: A valid object of SafetyByteArray
    *            Unit: SafetyByteArray
    *            Scaling: 1
    *            
    * @return void [out]   
    * @see mData
    *         
    *            
    */ 
   public void setData( SafetyByteArray data)
   {
       mData = data;
   } 
   
   
   /**
    * Get data parameter in request parameter.
    * 
    * @param N/A
    * 
    * @return mData: 
    *            Range: A valid object of SafetyByteArray
    *            Unit: SafetyByteArray
    *            Scaling: 1
    */ 
   public SafetyByteArray getData()
   {
       return mData;
   }
   
   /**
    * Get CommandCode parameter in request parameter.
    * 
    * @param N/A
    * 
    * @return CommandCode:  the request CommandCode
    *            Range: -2^31 to (2^31)-1 
    *            Unit: int 
    *            Scaling: 1 
    */
   public int getCommandCode()
   {
       return mCommandCode;
   }
   
   
   
   
   /**
    * Set CommandCode parameter in request parameter.
    * 
    * @param CommandCode: the request CommandCode
    *            Range: -2^31 to (2^31)-1 
    *            Unit: int 
    *            Scaling: 1 
    *            
    * @return void [out]          
    *            
    */ 
   public void setCommandCode(int CommandCode)
   {
       mCommandCode = CommandCode;
   }
   
   /**
    * Set UUID parameter in request parameter.
    * 
    * @param uuid: 
    *            Range: -2^31 to (2^31)-1 
    *            Unit: int 
    *            Scaling: 1 
    *            
    * @return void [out]          
    *            
    */ 
   public void setUUID(int uuid)
   {
       mUUID = uuid;
   }
   
   
   /**
    * Get OpCode parameter in request parameter.
    * 
    * @param N/A
    * 
    * @return OpCode: 
    *            Range: -2^31 to (2^31)-1 
    *            Unit: int 
    *            Scaling: 1 
    */
   public int getOpCode()
   {
       return mOpCode;
   }
   
   
   
   
   /**
    * Set OpCode parameter in request parameter.
    * 
    * @param uuid: 
    *            Range: -2^31 to (2^31)-1 
    *            Unit: int 
    *            Scaling: 1 
    *            
    * @return void [out]          
    *            
    */ 
   public void setOpCode(int opCode)
   {
       mOpCode = opCode;
   }
   
   
   /**
    * Get UUID parameter in request parameter.
    * 
    * @param N/A
    * 
    * @return uuid: 
    *            Range: -2^31 to (2^31)-1 
    *            Unit: int 
    *            Scaling: 1 
    */
   public int getUUID()
   {
       return mUUID;
   }
   
   
   /**
    * Set general parameter in request parameter.
    * 
    * @param parm: 
    *            Range: -2^31 to (2^31)-1 
    *            Unit: int 
    *            Scaling: 1 
    *            
    * @return void [out]           
    *            
    */
   public void setParameter(int parm)
   {
       mParm= parm;
   }
   
   
   /**
    * Get general parameter in request parameter.
    * 
    * @param N/A
    * 
    * @return mParm: 
    *            Range: -2^31 to (2^31)-1 
    *            Unit: int 
    *            Scaling: 1 
    */
   public int getParameter()
   {
       return mParm;
   }
   
   
   /**
    * Set general request
    * 
    * @param request: 
    *            Range: a valid object of IRequest 
    *            Unit: IRequest 
    *            Scaling: 1 
    *            
    * @return void [out]           
    *            
    */
   public void setRequest(IRequest request)
   {
       mRequest = request;
   }
   
   /**
    * Get general request parameter.
    * 
    * @param N/A
    * 
    * @return mRequest: 
    *            Range: a valid object of IRequest 
    *            Unit: IRequest 
    *            Scaling: 1 
    */
   public IRequest getRequest()
   {
       return mRequest;
   }
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */// (R18085 2015-09-16 05:09:21 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
