/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRInformation
 * Brief: 
 *
 * Create Date: 2015/5/25
 * $Revision: 20513 $
 * $Author: DWYang $
 * $Id: NotifyMessage.java 20513 2015-10-01 10:25:24Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.emwrservice;

import java.util.HashMap;
import java.util.Map;

import org.json.JSONException;
import org.json.JSONObject;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;

import android.util.Log;

public class NotifyMessage extends IEMWRInformation.Stub
{
    /**
     * The state of EMWR Message
     */
    public interface EMWR_MESSAGE_STATE
    {
        /**
         * state of waiting to show
         */
        int STATE_WAIT_TO_SHOW = HammingDistance.SAFETY_NUMBER_VALUE_0001;
        /**
         * state of showing notification
         */
        int STATE_SHOWING_NOTIFICATION = HammingDistance.SAFETY_NUMBER_VALUE_0002;
        /**
         * state of vibration
         */
        int STATE_SHOWING_VIBRATING = HammingDistance.SAFETY_NUMBER_VALUE_0003;
        /**
         * state of ringing
         */
        int STATE_SHOWING_RING = HammingDistance.SAFETY_NUMBER_VALUE_0004;
        /**
         * state of showing lock button
         */
        int STATE_SHOWING_LOCK_BUTTON = HammingDistance.SAFETY_NUMBER_VALUE_0005;
        /**
         * state of showing
         */
        int STATE_SHOWING = HammingDistance.SAFETY_NUMBER_VALUE_0006;
        /**
         * state of shown already
         */
        int STATE_SHOWN = HammingDistance.SAFETY_NUMBER_VALUE_0007;
        /**
         * state of confirmed
         */
        int STATE_CONFIRMED = HammingDistance.SAFETY_NUMBER_VALUE_0008;
        /**
         * state of closing for repeating
         */
        int STATE_CLOSED_FOR_REPEAT = HammingDistance.SAFETY_NUMBER_VALUE_0009;
    }
    
   /**
    * JSON name of notify item  
    */
   private static final String NOTIFY_ITEM = "notifyitem";
   /**
    * JSON name of center callback 
    */
   private static final String CENTER_CALLBACK = "centercallback";
   /**
    * JSON name of left callback 
    */
   private static final String LEFT_CALLBACK = "leftcallback";
   /**
    * JSON name of right callback 
    */
   private static final String RIGHT_CALLBACK = "rightcallback";
   /**
    * JSON name of sub content
    */
   private static final String SUB_CONTENT = "subcontent";
   /**
    * JSON name of custom reminder key
    */
   private static final String CUSTOM_REMINDER_KEY = "custom_reminderkey";

   /**
    * EMWR message in EMWR_LIST: default message is EMWR_LIST.WrongMessage.
    */
   private EMWRList mMessage = null;
   
   /**
    * the callback of center button  
    */
   private EMWRButtonCallback mCenterCallback = null;
   /**
    * the callback of left button 
    */
   private EMWRButtonCallback mLeftCallback = null;
   /**
    * the callback of right button 
    */
   private EMWRButtonCallback mRightCallback = null;
   
   /**
    * the button listeners of EMWR message
    */
//   private EMWRButtonCallback mButtonListener = null;
   
   /**
    * the state of the message
    * the value might be STATE_WAIT_TO_SHOW, STATE_SHOWING_LOCK_BUTTON,
    * STATE_SHOWING, STATE_SHOWN and STATE_CONFIRMED.
    */
   
   
   /**
    * The key of Custom reminder in SharePreference
    */
   private SafetyString mReminderKey = null;
   /**
    * the accessory text of the message
    */
   private SafetyString mSubContent = null;
   
   /**
    * Get the JSONObject string of message information. 
    * The string includes the callback status (center/left/right), sub content and message name.
    *
    * @param message NotifyMessage object that has the necessary information of EMWR message.
    * Range: valid NotifyMessage that exists message name
    * Unit: NotifyMessage
    * Scaling: 1
    * 
    * return SafetyString of JSONObject string.
    * Range: valid JSONObject object
    * Unit: JSONObject
    * Scaling: 1
    */
   public static SafetyString getMessageLine(NotifyMessage message)
   {
       Map<String, Object> jsonmap = new HashMap<String, Object>();
       JSONObject jsonObject = null;
       EMWRButtonCallback callback = null;
       boolean leftcallback = false;
       boolean rightcallback = false;
       boolean centercallback = false;
       SafetyString subcontent = null;
       SafetyString linestring = null;
       SafetyString reminderkey = null;
       
       
       // button listener
       callback = message.getCenterButtonClickListener();
       
       if(callback != null)
       {
           centercallback = true;
       }
       else
       {
           centercallback = false;
       }
       
       callback = message.getLeftButtonClickListener();
       
       if(callback != null)
       {
           leftcallback = true;
       }
       else
       {
           leftcallback = false;
       }
       
       callback = message.getRightButtonClickListener();
       
       if(callback != null)
       {
           rightcallback = true;
       }
       else
       {
           rightcallback = false;
       }
       
       // reminder key
       reminderkey = message.getCustomReminder();
       
       if(reminderkey != null)
       {
           jsonmap.put(NotifyMessage.CUSTOM_REMINDER_KEY, reminderkey.getString());
       }
       else
       {
           // Apply to the coding standard
       }
       
       // sub content
       subcontent = message.getString();
       
       if(subcontent != null)
       {
           jsonmap.put(NotifyMessage.SUB_CONTENT, message.getString().getString());
       }
       else
       {
           // Apply to the coding standard
       }
       
       jsonmap.put(NotifyMessage.NOTIFY_ITEM, message.getMessageItem().toString());
       
       jsonmap.put(NotifyMessage.CENTER_CALLBACK, centercallback);
       jsonmap.put(NotifyMessage.LEFT_CALLBACK, leftcallback);
       jsonmap.put(NotifyMessage.RIGHT_CALLBACK, rightcallback);
       
       jsonObject = new JSONObject(jsonmap);
       linestring = new SafetyString(jsonObject.toString(), CRCTool.generateCRC16(jsonObject.toString().getBytes()));
       
       return linestring;
   }
   
   /**
     * Parse JSONObject string, and return NotifyMessage object that includes the item in JSONObject string.
     * The function can parse notify item, sub content and the status of left/right/center callback.
     *
     * @param messageline A SafetyString that stores JSONObject.
     * Range: valid SafetyString object that meets JSONObject string format.
     * Unit: SafetyString
     * Scaling: 1
     * 
     * return NotifyMessage with message item, sub content and button callback if it has any.
     * Range: valid NotifyMessage object with message item
     * Unit: NotifyMessage
     * Scaling: 1
     */
    public static NotifyMessage getNotifyMessage(SafetyString messageline)
   {
       NotifyMessage message = null;
       
    try
    {
        EMWRList item = null;
        JSONObject jsonobject = new JSONObject(messageline.getString());
        String itemString = jsonobject.getString(NotifyMessage.NOTIFY_ITEM);
        String subString = jsonobject.optString(NotifyMessage.SUB_CONTENT, null);
        String reminderkey = jsonobject.optString(NotifyMessage.CUSTOM_REMINDER_KEY, null);
        boolean centercallback = jsonobject.getBoolean(NotifyMessage.CENTER_CALLBACK);
        boolean leftcallback = jsonobject.getBoolean(NotifyMessage.LEFT_CALLBACK);
        boolean rightcallback = jsonobject.getBoolean(NotifyMessage.RIGHT_CALLBACK);
        EMWRButtonCallback leftCallback = null;
        EMWRButtonCallback rightCallback = null;
        EMWRButtonCallback callback = new EMWRButtonCallback()
        {
            @Override
            public void onClick()
            {
                // Apply to the coding standard
            }
            
        };
        
        // create NotifyMessage
        //item = EMWRList.valueOf(itemString);
       	for (EMWRList p : EMWRList.values())
       	{
       		if( p.toString().equalsIgnoreCase(itemString) ){
       			item = p;
        	}
        }
        message = new NotifyMessage(item);

        if(reminderkey != null)
        {
            SafetyString safetykey = new SafetyString(reminderkey, CRCTool.generateCRC16(reminderkey.getBytes()));
            message.setCustomReminder(safetykey);
        }
        else
        {
            // Apply to the coding standard
        }
        
        if(subString != null)
        {
            SafetyString safetySub = new SafetyString(subString, CRCTool.generateCRC16(subString.getBytes()));
            message.setString(safetySub);
        }
        else
        {
           // Apply to the coding standard
        }
        
        if(leftcallback == true)
        {
            leftCallback = callback;
        }
        else
        {
            // Apply to the coding standard
        }
        
        if(rightcallback == true)
        {
            rightCallback = callback;
        }
        else
        {
            // Apply to the coding standard
        }
        
        message.setLeftRightButtonClickListener(leftCallback, rightCallback);
        
        if(centercallback == true)
        {
            Log.e("EMWR", "center callback");
            
            message.setCenterButtonClickListener(callback);
        }
        else
        {
            // Apply to the coding standard
        }
        
    }
    catch (JSONException e)
    {
        e.printStackTrace();
    }
    finally
    {
        // Apply to the coding standard
    }
       
       return message;
   }
   
   /**
    * EMWRMessageInformation constructor.
    * Set the EMWR message in an enumeration, EMWRList.
    * 
    * see mMessage [in] This global variable is referred for saving message item.
    * 
    * @param emwrMessage [in] a message in an enumeration, EMWRList. 
    * Range: valid item in EMWRList .
    * Unit: EMWRList
    * Scaling: 1
    * 
    * throw IllegalArgumentException if emwrMessage is not in the scope of EMWRList.
    * return None 
    */
   public NotifyMessage(EMWRList listItem)
   {
       CommonUtils.objectCheck(listItem);
       
       mMessage = listItem;
   }
   
   /**
    * Set clicking event of EMWR buttons.
    * 
    * @param listener [in] EMWRButtonCallback that defines
    *            buttons' event in a EMWR message.
    * @return None [out]
    */
//   @Override
//   void setButtonClickListener(EMWRButtonCallback listener)
//   {
//       if(listener != null)
//       {
//           mButtonListener = listener;
//       }
//       
//   }
   
   /**
    * Set reminder content by getting from SharePreference.
    * 
    *
    * @param shareKey SafetyString of custom reminder key.
    * Range: valid SafetyString object.
    * Unit: SafetyString
    * Scaling: 1
    * 
    * return None
    */
   public void setCustomReminder(SafetyString shareKey)
   {
       mReminderKey = shareKey;
   }
   
   /**
    * Get reminder content by getting from SharePreference.
    *
    * return SafetyString object is the key of custom reminder in SharePreference.
    */
   public SafetyString getCustomReminder()
   {
       return mReminderKey;
   }
   
   /**
     * Set the callback of center button.
     * 
     * see mLeftCallback [in] This global variable is referred for saving the callback of left button.
     * see mRightCallback [in] This global variable is referred for saving the callback of right button.
     * see mCenterCallback [in] This global variable is referred for saving the callback of center button.
     *
     * @param centerClick EMWRButtonCallback object that has the callback of center button.
     * Range: valid EMWRButtonCallback object.
     * Unit: EMWRButtonCallback
     * Scaling: 1
     * 
     * return None
     */
   public void setCenterButtonClickListener(EMWRButtonCallback centerClick)
   {
       if(centerClick != null)
       {
           mLeftCallback = null;
           mRightCallback = null;
           mCenterCallback = centerClick;
       }
       else
       {
           // Apply to the coding standard
       }
   }
   
   /**
     * Set the callback of left button and right button.
     * 
     * see mLeftCallback [in] This global variable is referred for saving the callback of left button.
     * see mRightCallback [in] This global variable is referred for saving the callback of right button.
     * see mCenterCallback [in] This global variable is referred for saving the callback of center button.
     * 
     * @param leftClick EMWRButtonCallback object that has the callback of left button.
     * Range: valid EMWRButtonCallback object.
     * Unit: EMWRButtonCallback
     * Scaling: 1
     * @param rightClick EMWRButtonCallback object that has the callback of right button.
     * Range: valid EMWRButtonCallback object.
     * Unit: EMWRButtonCallback
     * Scaling: 1
     * 
     * return None
     */
   public void setLeftRightButtonClickListener(EMWRButtonCallback leftClick, EMWRButtonCallback rightClick)
   {
       if((leftClick != null) || (rightClick != null))
       {
           mLeftCallback = leftClick;
           mRightCallback = rightClick;
           mCenterCallback = null;
       }
       else
       {
           // Apply to the coding standard
       }
   }
   
   /**
     * Get the callback of center button.
     *
     * return EMWRButtonCallback of center callback
     * Range: valid EMWRButtonCallback object.
     * Unit: EMWRButtonCallback
     * Scaling: 1
     */
    public EMWRButtonCallback getCenterButtonClickListener()
   {
       return mCenterCallback;
   }
   
    /**
     * Get the callback of left button.
     *
     * return EMWRButtonCallback of left callback
     * Range: valid EMWRButtonCallback object.
     * Unit: EMWRButtonCallback
     * Scaling: 1
     */
   public EMWRButtonCallback getLeftButtonClickListener()
   {
       return mLeftCallback;
   }
   
   /**
    * Get the callback of right button.
    *
    * return EMWRButtonCallback of right callback
    * Range: valid EMWRButtonCallback object.
    * Unit: EMWRButtonCallback
    * Scaling: 1
    */
   public EMWRButtonCallback getRightButtonClickListener()
   {
       return mRightCallback;
   }
   
   /**
    * Get the message item in EMWRList.
    * 
    * see mMessage [in] This global variable is referred for getting message item.
    * 
    * @param None [in]
    * 
    * return EMWRList Return the message item in EMWRList.
    * Range: valid item in EMWRList.
    * Unit: EMWRList
    * Scaling: 1
    */
   public EMWRList getMessageItem()
   {
       return mMessage;
   }
   
   /**
     * Set auxiliary text in EMWR message.
     * 
     * see mSubContent [in] This global variable is referred for saving sub content.
     *
     * @param subContent SafetyString object that stores the text of auxiliary text.
     * Range: valid SafetyString object.
     * Unit: SafetyString
     * Scaling: 1
     * 
     * return None
     */
   public void setString(SafetyString subContent)
   {
       mSubContent = subContent;
   }
   
   /**
    * Get auxiliary text in EMWR message.
    * 
    * see mSubContent [in] This global variable is referred for getting sub content.
    *
    * return SafetyString object that stores the text of auxiliary text.
    * Range: valid SafetyString object.
    * Unit: SafetyString
    * Scaling: 1
    */
   public SafetyString getString()
   {
       return mSubContent;
   }
   
    /**
     * Get a string that meets JSONObject format.
     *
     * return SafetyString that includes message name, the callback status of the message and sub content.
     * Range: valid SafetyString object.
     * Unit: SafetyString
     * Scaling: 1
     */
    @Override
    public SafetyString getMessageInfoString()
    {
        return getMessageLine(this);
    }

    /**
     * Execute the callback of left button.
     *
     * see mLeftCallback [in] This global variable is referred for getting the callback of left button.
     *
     * return None
     */
    @Override
    public void executeLeftButton()
    {
        if(mLeftCallback != null)
        {
            mLeftCallback.onClick();
        }
        else
        {
            // Apply to the coding standard
        }
    }
    
    /**
     * Execute the callback of right button.
     *
     * see mRightCallback [in] This global variable is referred for getting the callback of right button.
     * 
     * return None
     */
    @Override
    public void executeRightButton()
    {
        if(mRightCallback != null)
        {
            mRightCallback.onClick();
        }
        else
        {
            // Apply to the coding standard
        }
    }

    /**
     * Execute the callback of center button.
     *
     * see mCenterCallback [in] This global variable is referred for getting the callback of center button.
     *
     * return None
     */
    @Override
    public void executeCenterButton()
    {
        if(mCenterCallback != null)
        {
            mCenterCallback.onClick();
        }
        else
        {
            // Apply to the coding standard
        }
    }
}
