/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: SCR0382_quick_info_dropdown
 * Brief: Handle status bar and quick info
 *
 * Create Date: 07/08/2015
 * $Revision: 24473 $
 * $Author: AdamChen $
 * $Id: SCR0382_quick_info_dropdown.java 24473 2015-11-20 03:10:32Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.globaltools;

import android.app.Activity;
import android.app.Fragment;
import android.bluetooth.BluetoothAdapter;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.BatteryManager;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewGroup.LayoutParams;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import com.accu_chek.solo_m.rcapp.application.common.HammingValues;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.config.ConfigParameter;
import com.accu_chek.solo_m.rcapp.application.config.ReadConfig;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.ButtonBasic;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.Button_B29_2;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;
import com.sothree.slidinguppanel.SlidingUpPanelLayout;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;

public class SCR0382_quick_info_dropdown extends Fragment implements
        SlidingUpPanelLayout.PanelSlideListener
{

    //Debug used: It will be removed in the re;ease version
    private static final String TAG = SCR0382_quick_info_dropdown.class
            .getSimpleName();
    
    /**
     * Handle time information in the status bar.
     */
    public Handler mHandler = new Handler()
    {
        @Override
        public void handleMessage(Message msg)
        {
            mTimeViewSmall.setText(GlobalContainer.getInstance().timeformatted);
            checkStatusBarChanges();
        }
    };
    
    private View mRootView = null;
    
    private View mLayoutStatusBar = null;
    
    private View mDrawerLine = null;
    
    private int mColorActive = 0;
    
    private int mColorStatic = 0;
    
    private int mBatteryResource = 0;
    
    private TextView mTimeViewSmall = null;
    
    private ImageView mFlightPlaneMode = null;
    
    private ImageView mBluetooth = null;
    
    private ImageView mSoundMode = null;
    
//    private ImageView mTemperature = null;
    
    private ImageView mSuspendStatus = null;
    
    private ViewGroup mButtonContainer = null;
    
    private BluetoothAdapter mBluetoothAdapter = null;

    private TextView mTimeView = null;
    
    private TextView mDateView = null;
    
    private Context mContext = null;
       
    /**
     * 
     * Function Description
     *
     * @param screenName
     * @return
     * @return View.OnClickListener [out] Delete pre line return if exist. Parameter Description
     */
    public View.OnClickListener getGenericHandler(final String screenName)
    {

        return new View.OnClickListener()
        {

            @Override
            public void onClick(View v)
            {
                Toast.makeText(SCR0382_quick_info_dropdown.this.getActivity(),
                        "should go to " + screenName, Toast.LENGTH_SHORT)
                        .show();

            }
        };

    }

    /**
     * 
     * Called to have the fragment instantiate its user interface view.
     *
     * @param inflater [in]
     * @param container [in]
     * @param savedInstanceState [in]
     * @return View [out]
     */
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState)
    {
        View margin = new View(getActivity());
        int width = LayoutParams.MATCH_PARENT;
        int height = 10;
        View actionBar = null;
        
        Debug.printI(TAG, "[Enter] onCreateView()");

        // Inflate a new view hierarchy from the specified xml resource
        mRootView = inflater.inflate(R.layout.lad00qi, container, false);
        actionBar = mRootView.findViewById(R.id.layoutActionBar5_ref);
        // Get button container instance by resource id
        mButtonContainer = (ViewGroup) mRootView
                .findViewById(R.id.lad00qi_tableLayout);    
        // Set the layout parameters associated with this view.
        margin.setLayoutParams(new LayoutParams(width, height));
        // Adds a child view with the specified layout parameters.
        mButtonContainer.addView(margin);
        // add information to Quick info
        addButtons(new Button_B29_2(R.string.txt_dailyinsulintotal,
                R.drawable.total_40x40, "10.0 U",
                getGenericHandler("SCR0200_daily_totals")));

        // Add Time information in action bar
        mTimeView = (TextView) actionBar.findViewById(R.id.actionbar_title_text);
        mTimeView.setText(GlobalContainer.getInstance().timeformatted);

        // Add Time information in status bar
        mTimeViewSmall = (TextView) mRootView
                .findViewById(R.id.textViewStatusBarTime);
        GlobalContainer gt = GlobalContainer.getInstance();
        gt.registerTimeHandler(mHandler);
        mTimeViewSmall.setText(gt.timeformatted);

        // Get Battery view instance by resource id.
        gt.mBatteryView = (ImageView) mRootView
                .findViewById(R.id.imageViewStatusBarBattery);
        // Get Flight plane view instance by resource id.
        mFlightPlaneMode = (ImageView) mRootView
                .findViewById(R.id.imageViewStatusBarFlight);
        // Get Bluetooth view instance by resource id.
        mBluetooth = (ImageView) mRootView
                .findViewById(R.id.imageViewStatusBarBluetooth);
        // Get Sound view instance by resource id.
        mSoundMode = (ImageView) mRootView.findViewById(R.id.imageViewSlidingPanelStatusBarSilent);
        
//        mTemperature = (ImageView) mRootView.findViewById(R.id.imageViewStatusBarTemperature);
        
        mSuspendStatus = (ImageView) mRootView.findViewById(R.id.imageViewStatusBarSignalSuspension);

        
        mDateView = (TextView) actionBar.findViewById(R.id.actionbar_text);
        mDateView.setText("");
       
        mLayoutStatusBar = mRootView.findViewById(R.id.handle);
        mDrawerLine = mRootView.findViewById(R.id.drawer_line);
        
        mColorActive = getResources().getColor(R.color.android_blue);
        mColorStatic = getResources().getColor(R.color.main_grey);
        
        // add information in status bar.
        checkStatusBarChanges();
        
        return mRootView;
    }

    /**
     * 
     * Function Description
     *
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void updateBatteryLevel()
    {
        int batlevel = 0;
        GlobalContainer gt = GlobalContainer.getInstance();
        
        Debug.printI(TAG, "[updateBatteryLevel] mContext = " + mContext);

        Intent batteryIntent = mContext
                .registerReceiver(null,
                        new IntentFilter(Intent.ACTION_BATTERY_CHANGED));

        int status = batteryIntent.getIntExtra(BatteryManager.EXTRA_STATUS, -1);
        if (status == BatteryManager.BATTERY_STATUS_CHARGING)
        {
            mBatteryResource = R.drawable.status_battery_charging_20x30px;
            gt.mBatteryView.setImageResource(mBatteryResource);
            return;
        }

        batlevel = batteryIntent.getIntExtra(BatteryManager.EXTRA_LEVEL, -1);
        gt.mBatteryView.setImageResource(GlobalTools.getInstance()
                .getBatteryResourceLevel(batlevel));
    }
    
    
    
    @Override
	public void onCreate(Bundle savedInstanceState) {
		// TODO Auto-generated method stub
		super.onCreate(savedInstanceState);
		
		mContext = this.getActivity().getApplicationContext();
		
	}

	/**
     * 
     * 
     *
     */
    @Override
    public void onResume()
    {

        Debug.printI(TAG, "[Enter] onResume()");

        super.onResume();
        GlobalContainer gt = GlobalContainer.getInstance();
        gt.registerTimeHandler(mHandler);
        // timeViewSmall.setText(gt.timeformatted);
        String time = CommonUtils.getFormatTime(getActivity(),
                Calendar.getInstance().getTimeInMillis()).getString();
        String date = CommonUtils.getFormatDate(getActivity(), Calendar
                .getInstance().getTimeInMillis());
        mTimeViewSmall.setText(time);
        mTimeView.setText(time);
        mDateView.setText(date);

        checkStatusBarChanges();
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onDestroy()
    {
        super.onDestroy();
        mButtonContainer.removeAllViews();
    }

    /**
     * Adds button(s) to the container.
     * 
     * @param buttons
     *            : one or many buttons to be added to the button container
     */
    public void addButtons(ButtonBasic... buttons)
    {
        addButtons(Arrays.asList(buttons));
    }

    /**
     * Adds button(s) to the container.
     * 
     * @param buttons
     *            : one or many buttons to be added to the button container
     */
    public void addButtons(Collection<? extends ButtonBasic> buttons)
    {
        Activity activity = getActivity();
        for (ButtonBasic button : buttons)
        {
            mButtonContainer.addView(button.createView(
                    activity.getLayoutInflater(), null));
        }

    }

    /**
     * 
     * 
     *
     * @param panel
     * @param slideOffset
     */
    @Override
    public void onPanelSlide(View panel, float slideOffset)
    {
        mLayoutStatusBar.setVisibility(View.INVISIBLE);
        mDrawerLine.setVisibility(View.VISIBLE);
        mDrawerLine.setBackgroundColor(mColorActive);
        // scrollViewSlidingPanelContent.setY(-view.getY());
        // layoutSlidingPanelContentStatusBar.setY(-view.getY());

    }

    /**
     * 
     * 
     *
     * @param panel
     */
    @Override
    public void onPanelCollapsed(View panel)
    {

        mDrawerLine.setVisibility(View.INVISIBLE);
        mLayoutStatusBar.setVisibility(View.VISIBLE);

    }

    /**
     * 
     * 
     *
     * @param panel
     */
    @Override
    public void onPanelExpanded(View panel)
    {

        mDrawerLine.setBackgroundColor(mColorStatic);

    }

    /**
     * 
     * 
     *
     * @param panel
     */
    @Override
    public void onPanelAnchored(View panel)
    {
        // TODO Auto-generated method stub

    }

    /**
     * 
     * 
     *
     * @param panel
     */
    @Override
    public void onPanelHidden(View panel)
    {

    }
    
    /**
     * 
     * Function Description
     *
     */
    private void checkStatusBarChanges()
    {
        checkAirplaneModeOn();
        updateBatteryLevel();
        checkBluetoothenabled();
        checkSoundMode();
        checkSuspendStatus();
    }

    /**
     * 
     * Check flightmode status
     *
     * @return void [out] None
     */
    private void checkAirplaneModeOn()
    {

        // TODO
        byte filghtModeStatus = NugenSettingModel.getSafetyBoolean(
                mContext, NugenFrameworkConstants.KEY_FLIGHT_MODE_STATUS)
                .getByte();
        Debug.printI(TAG, "[checkAirplaneModeOn] mContext = " + mContext);
        if (filghtModeStatus == SafetyBoolean.TRUE.getByte())
        {
            mFlightPlaneMode.setImageResource(R.drawable.status_flight_mode);
        }
        else if (filghtModeStatus == SafetyBoolean.FALSE.getByte())
        {
            mFlightPlaneMode.setImageResource(0);
            
        }
        else
        {
            // Empty for code analysis
        }
        Debug.printI(TAG, "[checkAirplaneModeOn] exit ");
    }

    /**
     * 
     * Check the bluetooth status
     *
     * @return void [out] None
     */
    private void checkBluetoothenabled()
    {
        mBluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
        if (mBluetoothAdapter == null)
        {
            mBluetooth.setImageResource(R.drawable.status_no_comms);
        }
        else
        {
            if (!mBluetoothAdapter.isEnabled())
            {
                mBluetooth.setImageResource(android.R.color.transparent);
            }
        }
    }

    /**
     * 
     * Check the sound mode
     *
     * @return void [out] None
     */
    private void checkSoundMode()
    {
        
        final SafetyString sKey1 = new SafetyString(
                ConfigParameter.KEY_SIGNALIZATION_MODE,
                CRCTool.generateCRC16(ConfigParameter.KEY_SIGNALIZATION_MODE.getBytes()));
        
        final SafetyNumber<Integer> safetySignalCode = ReadConfig.getIntegerDataByKey(sKey1);
        
        CommonUtils.objectCheck(safetySignalCode);
        
        final int signalCode = safetySignalCode.get();
        Debug.printD(TAG, "signalCode : " + signalCode);
        
        // Display silent icon when the sound setting is silent
        if (signalCode == HammingValues.HAMMING_HD4_VALUE_0035)
        {
            mSoundMode.setImageResource(R.drawable.status_silent);
        }
        else 
        {
            mSoundMode.setImageResource(android.R.color.transparent);
        }
    }
    
    /**
     * 
     * Check the suspend status
     *
     * @return void [out] None
     */
    private void checkSuspendStatus()
    {
        final SafetyString sKey1 = new SafetyString(
                ConfigParameter.KEY_SIGNAL_SUSPEND_ENABLE,
                CRCTool.generateCRC16(ConfigParameter.KEY_SIGNAL_SUSPEND_ENABLE.getBytes()));
        
        final SafetyNumber<Integer> safetySSuspendStatus = ReadConfig.getIntegerDataByKey(sKey1);
        
        CommonUtils.objectCheck(safetySSuspendStatus);
        
        final int suspendStatus = safetySSuspendStatus.get();
        Debug.printD(TAG, "suspendStatus : " + suspendStatus);
        
        // display the suspend icon when SignalizationProfileWSuspendEnable is disable
        if (suspendStatus == HammingValues.HAMMING_HD4_VALUE_0097)
        {
            mSuspendStatus.setImageResource(R.drawable.status_signal_suspension);
        }
        else if (suspendStatus == HammingValues.HAMMING_HD4_VALUE_0096)
        {
            mSuspendStatus.setImageResource(android.R.color.transparent);
        }
        else
        {
            // Empty for code analysis
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
// [NSIQ-55] Disable "FrontManager" function to solve the memory leak problem.
// [GUI] update GUI framework to ClickThrough v0.34
