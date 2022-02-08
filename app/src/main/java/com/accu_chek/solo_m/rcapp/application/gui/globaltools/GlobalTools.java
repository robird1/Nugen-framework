/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: GlobalTools
 * Brief: This class provide the common tool for all module in RC app
 * 
 * Create Date: 05/27/2015
 * $Revision: 24973 $
 * $Author: AdamChen $
 * $Id: GlobalTools.java 24973 2015-11-27 06:03:35Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.globaltools;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.content.res.Resources;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.widget.ImageView;
import android.widget.TextView;

import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.CommonConstants;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.NotificationCenter;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.StringHelper;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Calendar;


public class GlobalTools
{

    private static final int BATLEVEL1_LOWER = 0;

    private static final int BATLEVEL1_UPPER = 20;

    private static final int BATLEVEL2_LOWER = 20;

    private static final int BATLEVEL2_UPPER = 30;

    private static final int BATLEVEL3_LOWER = 30;

    private static final int BATLEVEL3_UPPER = 50;

    private static final int BATLEVEL4_LOWER = 50;

    private static final int BATLEVEL4_UPPER = 70;

    private static final int BATLEVEL5_LOWER = 70;

    private static final int BATLEVEL5_UPPER = 90;

    private static final int BATLEVEL6_LOWER = 90;

    private static final int BATLEVEL6_UPPER = 100;

    // For debug used, it would be removed in the release version
//    private static final String TAG = "GlobalTools";

    public static int Picker_Delay = 75;

    public static boolean ReplaceLenghtWithDots = false;
    
    // Singleton pattern: GlobalTools reference
    private static volatile GlobalTools mInstance;

    // Context reference
    private Context mContext;

    // Global variable
    private boolean mPump_state = true;

    private boolean mFlightmode_active = false;

    private boolean mBle_connect_state = false;

    private boolean mBle_bond_state = false;

    // Helper for dynamic variation (increase/decrease basal rate, see SCR0115).
    private int mTotalTempBasalRate = 105;

    // current Application mode (MDI or MicroPump)
    private Mode mCurrentMode = Mode.MicroPump;

    // Modified by Henry Tso
    private SafetyNumber<Integer> mSCR0206_picker_carbs_value = null;

    // Added by Henry Tso
    private SafetyNumber<Integer> mSCR0183_meal_time_index = null;

    // Array of Hamming value. Safety issue is considered
    private int[] mSCR0092_health_events = null;

    // Used to adjust Hardware Effectiveness
    private boolean mInvalid_Hardware = true;

    private int hypolimit = 0;

    private int hyperlimit = 0;

    private int lowlimit = 0;

    private int highlimit = 0;

    // Restrict the constructor from being instantiated
    private GlobalTools()
    {
        /**
         * Do nothing here and just
         * force other module use
         * the static method getInstance
         * to get this instance.
         */
    }

    /**
     * 
     * Get bg value from resource
     *
     * @param context [in]
     * 
     */
    public void init(Context context)
    {
        this.mContext = context;
        hyperlimit = context.getResources()
                .getInteger(R.integer.bg_hypersymbol);
        hypolimit = context.getResources().getInteger(R.integer.bg_hypo);
        lowlimit = context.getResources().getInteger(R.integer.bg_low);
        highlimit = context.getResources().getInteger(R.integer.bg_high);
    }

    /**
     * 
     * set Pump status
     *
     * @param d [in]
     * 
     */
    public void setPumpState(boolean d)
    {
        this.mPump_state = d;
    }

    /**
     * 
     * get Pump status
     *
     * 
     * @return boolean [out]
     */
    public boolean getPumpState()
    {
        return this.mPump_state;
    }

    /**
     * 
     * Set Flight mode status
     *
     * @param d [in]
     * 
     */
    public void setFlightState(boolean d)
    {
        this.mFlightmode_active = d;
    }

    /**
     * 
     * get Flight mode status
     *
     * 
     * @return boolean [out]
     */
    public boolean getFlightState()
    {
        return this.mFlightmode_active;
    }

    /**
     * 
     * Function Description
     *
     * @param d
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setBLEConnectState(boolean d)
    {
        this.mBle_connect_state = d;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return boolean [out] Delete pre line return if exist. Parameter
     *         Description
     */
    public boolean getBLEConnectState()
    {
        return this.mBle_connect_state;
    }

    /**
     * 
     * Function Description
     *
     * @param d
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setBLEBondState(boolean d)
    {
        this.mBle_bond_state = d;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return boolean [out] Delete pre line return if exist. Parameter
     *         Description
     */
    public boolean getBLEBondState()
    {
        return this.mBle_bond_state;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return GlobalTools [out] Delete pre line return if exist. Parameter
     *         Description
     */
    public static synchronized GlobalTools getInstance()
    {
        if (mInstance == null)
        {
            mInstance = new GlobalTools();
        }
        return mInstance;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getTotalTempBasalRate()
    {
        return mTotalTempBasalRate;
    }

    /**
     * 
     * Function Description
     *
     * @param totalTempBasalRate
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setTotalTempBasalRate(int totalTempBasalRate)
    {
        this.mTotalTempBasalRate = totalTempBasalRate;
    }

    /**
     * Get UI mode (Pump/MDI)
     * 
     * @return the current mode
     */
    public Mode getMode()
    {
        return mCurrentMode;
    }

    /**
     * Set UI mode (Pump/MDI)
     * 
     * @param newMode
     *            the mode to set
     */
    public void setMode(Mode newMode)
    {
//        Debug.printI(TAG, "[Enter] setMode()");
        this.mCurrentMode = newMode;

//        Debug.printI(TAG, "newMode: "+ newMode.toString());

        // Put current mode in user setting module.
        String mode = newMode.toString();
        NugenSettingModel.setString(mContext, CommonConstants.KEY_SYSTEM_MODE,
                new SafetyString(mode, CRCTool.generateCRC16(mode.getBytes())));

    }

    /**
     * 
     * Set bg value and check bg value for show the specified string.
     *
     * @param bgvalue [in]
     * 
     * @return String [out]
     */
    public String setAndCheckValue(String bgvalue)
    {
        int value = Integer.parseInt(bgvalue);
        Resources res = mContext.getResources();
        String ret = bgvalue;
        int bgLowValue = res.getInteger(R.integer.bg_low);
        int bgHighValue = res.getInteger(R.integer.bg_high);

        if (value <= bgLowValue)
        {
            ret = res.getString(R.string.txt_lo);
        }
        else if (value >= bgHighValue)
        {
            ret = res.getString(R.string.txt_hi);
        }

        return ret;
    }

    /**
     * 
     * Provide the interface that check bg value high or low
     *
     * @param value [in]
     * 
     * @return Boolean [out]
     */
    public Boolean isHighLow(String value)
    {
        Resources res = mContext.getResources();
        boolean ret = false;

        boolean isEqualToLo = value.equals(res.getString(R.string.txt_lo));
        boolean isEqualToHigh = value.equals(res.getString(R.string.txt_hi));

        if (isEqualToLo || isEqualToHigh)
        {
            ret = true;
        }

        return ret;
    }

    /**
     * 
     * Used to get Circle icon by bg value.
     *
     * @param bgvalue [in]
     *
     * @return int [out]
     */
    public int getCircleDrawable(String bgvalue)
    {
        int ret = 0;
        int bgValue = Integer.parseInt(bgvalue);
        Resources res = mContext.getResources();
        int bgLowValue = res.getInteger(R.integer.bg_low);
        int bgHypoValue = res.getInteger(R.integer.bg_hypo);
        int bgTargetValue = res.getInteger(R.integer.bg_target);
        int bgHyperValue = res.getInteger(R.integer.bg_hyper);
        int bgHighValue = res.getInteger(R.integer.bg_high);

        if (bgvalue.length() == 0)
        {
            ret = R.drawable.circle_bg_low;
        }

        if (bgValue <= bgLowValue)
        {
            ret = 0;
        }
        else if (bgValue <= bgHypoValue)
        {
            ret = R.drawable.circle_bg_hypo;
        }
        else if (bgValue <= bgTargetValue)
        {
            ret = R.drawable.circle_bg_low;
        }
        else if (bgValue <= bgHyperValue)
        {
            ret = R.drawable.circle_bg_target;
        }
        else if (bgValue <= bgHighValue)
        {
            ret = R.drawable.circle_bg_high;
        }

        return ret;
    }

    /**
     * 
     * Get date formate by Calendar instance.
     *
     * @return
     * @return String [out] Delete pre line return if exist. Parameter
     *         Description
     */
    public String getDateFormatted()
    {
        // SimpleDateFormat sdf = new SimpleDateFormat("HH:mm a, dd MMM. yyyy");
        Calendar cal = Calendar.getInstance();
        String time = StringHelper.formatTime(cal.get(Calendar.HOUR),
                cal.get(Calendar.MINUTE));
        String date = StringHelper.formatDate(cal.get(Calendar.DAY_OF_MONTH),
                cal.get(Calendar.MONTH) + 1, cal.get(Calendar.YEAR));
        String strDate = time + ", " + date;

        return strDate;
    }

    /**
     * 
     * Get the Hyper and Hypo icon by bg value
     *
     * @param value [in]
     * @param need_white [in]
     * 
     * @return Drawable [out]
     */
    public Drawable getHyperHypoSymbol(int value, boolean need_white)
    {
        Resources res = mContext.getResources();
        Drawable ret = null;

        if ((value > hyperlimit) && (value <= highlimit))
        {
            if (need_white)
            {
                Drawable img = res.getDrawable(R.drawable.status_hyper_white);
                ret = img;
            }
            else
            {
                Drawable img = res.getDrawable(R.drawable.status_hyper_grey);
                ret = img;
            }
        }

        if ((value < hypolimit) && (value > lowlimit))
        {
            if (need_white)
            {
                Drawable img = res.getDrawable(R.drawable.status_hypo_white);
                ret = img;
            }
            else
            {
                Drawable img = res.getDrawable(R.drawable.status_hypo_grey);
                ret = img;
            }
        }

        return ret;
    }

    /**
     * 
     * Provide the interface that generate random value.
     *
     * @param range [in]
     * 
     * @return int [out]
     */
    public int randomValue(int range)
    {
        int count = 0;
        SecureRandom random = null;
        
        try
        {
            random = SecureRandom.getInstance("SHA1PRNG");
            random.setSeed(1);          
            count = random.nextInt(range);
            
        }
        catch (NoSuchAlgorithmException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard.
        }

        return count;
    }

    /**
     * 
     * Check error condition
     *
     * 
     * @return boolean [out]
     */
    public boolean checkIfErrorRaised()
    {
        if (mInvalid_Hardware)
        {
            NotificationCenter nc = NotificationCenter.getInstance();
            nc.create_B29_notify("ERROR!!!", "Hardware not compatible",
                    "No function of LEDs", "Measurements and Vibrations",
                    R.drawable.error, NotificationCenter.KEY_ERROR);
        }
        return mInvalid_Hardware;
    }

    /**
     * 
     * Create error dialog
     *
     * @param act [in]
     * @param err_txt [in]
     * @return void [out]
     */
    public void errorDialogBox(Activity act, String err_txt)
    {
        new AlertDialog.Builder(act).setTitle("Error!!")
                .setMessage("Your Hardware is not supported.\n" + err_txt)
                .setCancelable(false)
                .setPositiveButton("ok", new OnClickListener()
                {
                    @Override
                    public void onClick(DialogInterface dialog, int which)
                    {

                    }
                }).create().show();

    }

    /**
     * 
     * Create error dialog
     *
     * @param act [in]
     * @param err_txt [in]
     * @param dialog [in]
     * @return void [out]
     */
    public void errorDialogBox(Activity act, String err_txt,
            OnClickListener dialog)
    {
        new AlertDialog.Builder(act).setTitle("Error!!")
                .setMessage("Your Hardware is not supported.\n" + err_txt)
                .setCancelable(false).setPositiveButton("ok", dialog)
                .setNegativeButton("cancel", new OnClickListener()
                {
                    @Override
                    public void onClick(DialogInterface dialog, int which)
                    {

                    }
                }).create().show();

    }

    /**
     * 
     * Set the ellipsize property of the textview
     *
     * @param tv [in]
     *
     */
    public void setEllipsize(TextView tv)
    {
        tv.setSingleLine(true);
        tv.setEllipsize(TextUtils.TruncateAt.END);
    }

    /**
     * 
     * Get battery level icon by battery level value
     *
     * @param batlevel [in]
     * 
     * @return int [out]
     */
    public int getBatteryResourceLevel(int batlevel)
    {
        int mBatteryResource = 0;

        if ((batlevel <= BATLEVEL6_UPPER) && (batlevel >= BATLEVEL6_LOWER))
        {
            mBatteryResource = R.drawable.status_battery_100_20x30px;
        }
        else if ((batlevel < BATLEVEL5_UPPER) && (batlevel >= BATLEVEL5_LOWER))
        {
            mBatteryResource = R.drawable.status_battery_80_20x30px;
        }
        else if ((batlevel < BATLEVEL4_UPPER) && (batlevel >= BATLEVEL4_LOWER))
        {
            mBatteryResource = R.drawable.status_battery_60_20x30px;
        }
        else if ((batlevel < BATLEVEL3_UPPER) && (batlevel >= BATLEVEL3_LOWER))
        {
            mBatteryResource = R.drawable.status_battery_40_20x30px;
        }
        else if ((batlevel < BATLEVEL2_UPPER) && (batlevel >= BATLEVEL2_LOWER))
        {
            mBatteryResource = R.drawable.status_battery_20_20x30px;
        }
        else if ((batlevel < BATLEVEL1_UPPER) && (batlevel >= BATLEVEL1_LOWER))
        {
            mBatteryResource = R.drawable.status_battery_low_20x30px;
        }
        else
        {
            mBatteryResource = R.drawable.status_signal_suspension;
        }

        return mBatteryResource;
    }

    /**
     * Call this API to get the Circle drawable resource ID of the given bG
     * result
     * Modified by Henry Tso
     * 
     * @param view [in] ImageView object of the UI component for displaying the
     *            circle
     *            Range: Valid ImageView object
     *            Unit: ImageView
     *            Scaling: 1
     * @param bgvalue [in] String of the bG result
     *            Range: -2^31 to (2^31)-1
     *            Unit: int
     *            Scaling: 1
     * @return int [out] Drawable resource ID
     *         Range: -2^31 to (2^31)-1
     *         Unit: int
     *         Scaling: 1
     */
    public int getCircleDrawable(ImageView view, String bgvalue)
    {
        int ret = 0;
        int value = Integer.parseInt(bgvalue);
        Resources res = mContext.getResources();
        int length = bgvalue.length();
        int bgLowValue = res.getInteger(R.integer.bg_low);
        int bgHypoValue = res.getInteger(R.integer.bg_hypo);
        int bgTargetValue = res.getInteger(R.integer.bg_target);
        int bgHyperValue = res.getInteger(R.integer.bg_hyper);
        int bgHighValue = res.getInteger(R.integer.bg_high);

        if (length == 0)
        {
            view.setContentDescription("bGValue_LO");
            ret = R.drawable.circle_bg_low;
        }

        if (value <= bgLowValue)
        {
            view.setContentDescription("bGValue_LO");
            ret = 0;
        }
        else if (value <= bgHypoValue)
        {
            view.setContentDescription("bGValue_Hypo");
            ret = R.drawable.circle_bg_hypo;
        }
        else if (value <= bgTargetValue)
        {
            view.setContentDescription("bGValue_Target");
            ret = R.drawable.circle_bg_low;
        }
        else if (value <= bgHyperValue)
        {
            view.setContentDescription("bGValue_Hyper");
            ret = R.drawable.circle_bg_target;
        }
        else if (value <= bgHighValue)
        {
            view.setContentDescription("bGValue_High");
            ret = R.drawable.circle_bg_high;
        }
        else
        {
            ret = 0;
        }

        return ret;
    }

    /**
     * 
     * Function Description
     *
     * @param view
     * @param bgvalue
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int getCircleDrawable(ImageView view, int value)
    {
        int ret = 0;
        
        Resources res = mContext.getResources();
 
        if (value <= res.getInteger(R.integer.bg_low))
        {
            view.setContentDescription("bGValue_LO");
            ret = R.drawable.circle_bg_hypo;
        }
        else if (value <= res.getInteger(R.integer.bg_hypo))
        {
            view.setContentDescription("bGValue_LO");
            ret = R.drawable.circle_bg_hypo;
        }
        else if (value <= res.getInteger(R.integer.bg_target))
        {
            view.setContentDescription("bGValue_Hypo");
            ret = R.drawable.circle_bg_low;
        }
        else if (value <= res.getInteger(R.integer.bg_hyper))
        {
            view.setContentDescription("bGValue_Target");
            ret = R.drawable.circle_bg_target;
        }
        else if (value <= res.getInteger(R.integer.bg_high))
        {
            view.setContentDescription("bGValue_Hyper");
            ret = R.drawable.circle_bg_high;
        }
        else
        {
            ret = 0;
        }

        return ret;
    }

    // Added by Henry Tso
    /**
     * Getting function. For getting the input Carbs value from the
     * Carbs value picker Activity.
     * 
     * @return SafetyString [out] Safety value of the Carbs
     *         that input by the user.
     *         Range: Valid SafetyNumber<Integer> object
     *         Unit: SafetyNumber<Integer>
     *         Scaling: 1
     * 
     * @see SCR0206_picker_carbs_value
     */
    public SafetyNumber<Integer> getSCR0206_picker_carbs_value()
    {
        return mSCR0206_picker_carbs_value;
    }

    // Added by Henry Tso
    /**
     * Setting function. For setting the default Carbs Value to the
     * Carbs picker Activity
     * 
     * @param sValue [in] Initial value of the Carbs value. This value
     *            is a hamming value that represent the index of the Item.
     *            Range: Valid SafetyNumber<Integer> object. The valid range
     *            please
     *            refer to Configuration Matrix.
     *            Unit: SafetyNumber<Integer>
     *            Scaling: 1
     * 
     * @see SCR0206_picker_carbs_value
     */
    public void setSCR0206_picker_carbs_value(SafetyNumber<Integer> nValue)
    {
        mSCR0206_picker_carbs_value = nValue;
    }

    // Added by Henry Tso
    /**
     * Getting function. For getting the select index of the Meal time
     * Activity.
     * 
     * @return SafetyString [out] SafetyNumber<Integer> value of the
     *         meal time index.
     *         Range: Valid SafetyNumber<Integer> object
     *         MEAL_TIME_NO_ENTRY = 0x000F
     *         MEAL_TIME_BEFORE_MEAL = 0x0033
     *         MEAL_TIME_AFTER_MEAL = 0x003C
     *         MEAL_TIME_BEDTIME = 0x0055
     *         MEAL_TIME_FASTING = 0x005A
     *         MEAL_TIME_OTHER = 0x0066
     *         Unit: SafetyNumber<Integer>
     *         Scaling: 1
     * 
     * @see SCR0183_meal_time_index
     */
    public SafetyNumber<Integer> getSCR0183_meal_time_index()
    {
        return mSCR0183_meal_time_index;
    }

    /**
     * Setting function. For setting the initial index of the meal time
     * Activity
     * 
     * @param nValue [in] SafetyNumber<Integer>. The initial selected index
     *            of the item
     * 
     * @see SCR0183_meal_time_index
     */
    public void setSCR0183_meal_time_index(SafetyNumber<Integer> nValue)
    {
        mSCR0183_meal_time_index = nValue;
    }

    /**
     * Call this API for getting the selected items from the Health
     * Events Activity. The usage module must be checked the return
     * object is not null.
     * 
     * @return SafetyNumber<Integer>[] [out] Selected item list
     *         Range: Valid SafetyNumber<Integer> array object
     *         NO_VALUE=0x0F, EXERCISE1=0x33, EXERCISE2=0x3C,
     *         STRESS=0x55,ILLNESS=0x5A, PMS=0x66, CUSTOM1=0x69,
     *         CUSTOM2=0x96, CUSTOM3=0x99
     *         Unit: SafetyNumber<Integer> object
     *         Scaling: 1
     * 
     * @see SCR0092_health_events
     */
    public int[] getSCR0092_health_events_value()
    {
        return mSCR0092_health_events;
    }

    /**
     * Call this API for setting the selected items list of the
     * Health Events
     * 
     * @param nItemList [in] Selected items list of Health events.
     *            Range: Valid int array object
     *            NO_VALUE=0x0F, EXERCISE1=0x33, EXERCISE2=0x3C,
     *            STRESS=0x55,ILLNESS=0x5A, PMS=0x66, CUSTOM1=0x69,
     *            CUSTOM2=0x96, CUSTOM3=0x99
     *            Unit: int[]
     *            Scaling: 1
     * 
     * @see SCR0092_health_events
     */
    public void setSCR0092_health_events(int[] nItemList)
    {
        mSCR0092_health_events = nItemList;
    }

    /**
     * Call this API for returning the text of bG value with LO/HI check
     * 
     * @param bgvalue
     * @return
     * @return SafetyString [out] Delete pre line return if exist. Parameter
     *         Description
     */
    public SafetyString setAndCheckValue(SafetyNumber<Integer> bgvalue)
    {
        int value = bgvalue.get();
        Resources res = mContext.getResources();
        String sResult = "";
        int bgLowValue = res.getInteger(R.integer.bg_low);
        int bgHighValue = res.getInteger(R.integer.bg_high);

        if (value <= bgLowValue)
        {
            sResult = res.getString(R.string.txt_lo);
        }
        else if (value >= bgHighValue)
        {
            sResult = res.getString(R.string.txt_hi);
        }
        else
        {
            sResult = String.format("%d", value);
        }
        return new SafetyString(sResult, CRCTool.generateCRC16(sResult
                .getBytes()));
    }

    /**
     * Enumerates possible modes for the application
     * 
     */
    public enum Mode
    {
        MicroPump,
        MDI,
        FlightMode,
        Non_bg;
    }

    /**
     * 
     */
    public static abstract class MPR
    {

        static int responceFlag = HammingDistance.SAFETY_BOOLEAN_FALSE;

        static SafetyByteArray mpAddress = null;

        private static byte[] mPinCode = null;

        private static ScanType mScanType = null;

        private static byte[] mKey = null;
        
        private static byte[] mIDDStatus = null;

        private static SafetyBoolean mBonding = SafetyBoolean.FALSE;
        /**
         * 
         * Function Description
         *
         * @param flag
         * @return void [out] Delete pre line return if exist. Parameter
         *         Description
         */
        public static void setResponseFlag(int flag)
        {
            responceFlag = flag;
        }

        /**
         * 
         * Function Description
         *
         * @return
         * @return int [out] Delete pre line return if exist. Parameter
         *         Description
         */
        public static int getResponseFlag()
        {
            return responceFlag;
        }

        /**
         * 
         * Function Description
         *
         * @param address
         * @return void [out] Delete pre line return if exist. Parameter
         *         Description
         */
        public static void setMpAddress(SafetyByteArray address)
        {
            mpAddress = address;
        }

        /**
         * 
         * Function Description
         *
         * @return
         * @return SafetyByteArray [out] Delete pre line return if exist.
         *         Parameter Description
         */
        public static SafetyByteArray getMpAddress()
        {
            return mpAddress;
        }

        /**
         * 
         * Function Description
         *
         * @param key
         * @return void [out] Delete pre line return if exist. Parameter
         *         Description
         */
        public static void setKey(byte[] key)
        {
            mKey = key;
        }

        /**
         * 
         * Function Description
         *
         * @return
         * @return byte[] [out] Delete pre line return if exist. Parameter
         *         Description
         */
        public static byte[] getKey()
        {
            return mKey;
        }

        /**
         * 
         * Function Description
         *
         * @param pinCode
         * @return void [out] Delete pre line return if exist. Parameter
         *         Description
         */
        public static void setPinCode(byte[] pinCode)
        {
            mPinCode = pinCode;
        }

        /**
         * 
         * Function Description
         *
         * @return
         * @return String [out] Delete pre line return if exist. Parameter
         *         Description
         */
        public static byte[] getPinCode()
        {
            return mPinCode;
        }

        /**
         * 
         * Function Description
         *
         * @param pinCode
         * @return void [out] 
         */
        public static void setIDDStatus(byte[] iddStatus)
        {
            mIDDStatus = iddStatus;
        }

        /**
         * 
         * Function Description
         *
         * @return
         * @return String [out] Delete pre line return if exist. Parameter
         *         Description
         */
        public static byte[] getIDDStatus()
        {
            return mIDDStatus;
        }
        
        /**
         * 
         * Function Description
         *
         * @param pinCode
         * @return void [out] 
         */
        public static void setBondingStatus(SafetyBoolean bondingStatus)
        {
            mBonding = bondingStatus;
        }

        /**
         * 
         * Function Description
         *
         * @return
         * @return String [out] Delete pre line return if exist. Parameter
         *         Description
         */
        public static SafetyBoolean getBondingStatus()
        {
            return mBonding;
        }
        
        /**
         * 
         * Function Description
         *
         * @return
         * @return ScanType [out] Delete pre line return if exist. Parameter
         *         Description
         */
        public static ScanType getScanType()
        {
            return mScanType;
        }

        /**
         * 
         * Function Description
         *
         * @param type
         * @return void [out] Delete pre line return if exist. Parameter
         *         Description
         */
        public static void setScanType(ScanType type)
        {
            mScanType = type;
        }

        /**
         * Provide Scan Type
         */
        public static enum ScanType
        {
            enable,
            disable;
        }

    }

    /**
     * This class is only used to collect the relative information.
     */
    public static class ReplacePartInf
    {

        public static Boolean inf_set = false;

        public static Boolean res_set = false;

        public static Boolean mic_set = false;

    };

}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// [GUI] update GUI framework to ClickThrough v0.34
