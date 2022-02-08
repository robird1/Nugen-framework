/** 
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: AbstractFragmentViewPage
 * Brief: Provide the interface of the AbstractFragmentViewPage UI
 *
 * Create Date: 02/13/2015
 * $Revision: 25086 $
 * $Author: AdamChen $
 * $Id: AbstractFragmentViewPage.java 25086 2015-11-30 06:31:50Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.gui.interfaces;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v7.app.ActionBarActivity;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;

import com.accu_chek.solo_m.rcapp.application.gui.globaltools.FragmentChangeListener;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.CollectionPageAdapter;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.TopActionBar;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0010;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0010a;
import com.accu_chek.solo_m.rcapp.application.gui.uitools.layouts.LAD0021;
import com.accu_chek.solo_m.rcapp.application.safety.HammingDistance;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.rcframeworklibrary.R;

import java.lang.reflect.Method;

public abstract class AbstractFragmentViewPage implements ILayoutInit,
        IFooterButton, FragmentChangeListener
{
    
    private static final int ICON_TOTAL_NUMBER_ONE = 1;
    
    private static final int ICON_TOTAL_NUMBER_TWO = 2;
    
    private static final int TAB_INDEX_LEFT = 0;
    
    private static final int TAB_INDEX_RIGHT = 1;

    protected ActionBarActivity mContext = null;
    
    private CollectionPageAdapter mCollectionPagerAdapter = null;
    
    private Fragment mFragmentL = null;
    
    private Fragment mFragmentR = null;

    private int mRightTabIconId = R.drawable.table;

    private int mLeftTabIconId = R.drawable.barchart;

    private int mRActionBarIconId = R.drawable.delete_white;

    private int mActionBarText = R.string.txt_scr0206_numpad_carbs_title;

    private int mActionBarIconId = R.drawable.carbs_75x75px;

    private int mDefaultIconNumber = 1;

    // Added by Henry Tso
    private int mFrameType = 0;
    
    /**
     * 
     * @param context
     */
    public AbstractFragmentViewPage(final ActionBarActivity context)
    {
        mContext = context;
    }
    
    /**
     * 
     * 
     *
     * @param activity
     * @param data
     */
    @Override
    public void setupLayout(ActionBarActivity activity, Bundle data)
    {
        initLayout(onPageViewLayout());

        setupActionBar(onActionBarIconNumber());

    }

    /**
     * This function will only be called when the current layout type is Layout10
     * 
     */
    @Override
    public void onFragmentChanged(int position)
    {
        try
        {
            Method leftTabSetFC = mFragmentL.getClass().getDeclaredMethod(
                    "setFC");
            Method rightTabSetFC = mFragmentR.getClass().getDeclaredMethod(
                    "setFC");
            Method leftTabGetFC = mFragmentL.getClass().getDeclaredMethod(
                    "getFC");
            Method rightTabGetFC = mFragmentR.getClass().getDeclaredMethod(
                    "getFC");
            // Added by Henry Tso.
            mFrameType = position; 
            switch (position)
            {
            case TAB_INDEX_LEFT:
                rightTabSetFC.invoke(mFragmentR);
                leftTabGetFC.invoke(mFragmentL);

                break;
            case TAB_INDEX_RIGHT:

                leftTabSetFC.invoke(mFragmentL);
                rightTabGetFC.invoke(mFragmentR);

                break;
            default:
                // do nothing
                break;
            }
        }
        catch (Exception e)
        {
            throw new IllegalArgumentException();
        }
        finally
        {
            // Apply to the coding standard
        }
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void setFC()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void getFC()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }
    
    /**
     * 
     * Function Description
     *
     * @param layout
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void setupDateScroll(LAD0021 layout)
    {
        // override by subclass
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int onActionBarIconNumber()
    {
        return mDefaultIconNumber;
    }
    
    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int onLActionBarIconId()
    {
        return mActionBarIconId;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int onActionBarText()
    {
        return mActionBarText;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int onRActionBarIconId()
    {
        return mRActionBarIconId;
    }

    /**
     * 
     * Function Description
     *
     * @param v
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    public void onRActionBarIconClicked(View v)
    {        
        // override by subclass
    }
    
    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int onLeftTabIconId()
    {
        return mLeftTabIconId;
    }

    /**
     * 
     * Function Description
     *
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    public int onRightTabIconId()
    {
        return mRightTabIconId;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getFooterLeftButtonTextId()
    {
        return R.string.txt_labeldone;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getFooterRightButtonTextId()
    {
        return R.string.txt_label_activate;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getFooterMiddleButtonIconId()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        return 0;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getFooterButtonNumber()
    {
        return 1;
    }

    /**
     * 
     * 
     *
     * @return
     */
    @Override
    public int getFooterButtonTextId()
    {
        return R.string.txt_set;
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterLeftButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterRightButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onFooterMiddleButtonClicked()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     * @param activity
     * @param data
     * @param requestCode
     */
    @Override
    public void updateLayout(ActionBarActivity activity, Bundle data,
            int requestCode)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     * @param activity
     */
    @Override
    public void onBackPressed(ActionBarActivity activity)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }
    
    /**
     * 
     * 
     *
     * @param activity
     */
    @Override
    public void onHomePressed(ActionBarActivity activity)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }
    
    /**
     * 
     * 
     *
     * @param activity
     */
    @Override
    public void onNextPressed(ActionBarActivity activity)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     * @param activity
     */
    @Override
    public void onInsulinConfirmationPressed(ActionBarActivity activity)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     * @param savedInstanceState
     */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onStart()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onResume()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onPause()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onStop()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    /**
     * 
     * 
     *
     */
    @Override
    public void onDestroy()
    {
        /**
         * This method is forced to override the functionality of an existing
         * method of super class and no need to do action in this method.
         */
        
    }

    protected abstract int onPageViewLayout();
    
    protected abstract String onLeftTabClassName();
    
    protected abstract String onRightTabClassName();
    
    protected abstract String onScreenId();

    /**
     * Call this API to return the current displayed Fragment object
     * Added by Henry Tso
     *
     * @return Fragment [out] Current displayed Fragment object
     * Range: Valid Fragment object
     * Unit: Fragment
     * Scaling: 1
     */
    protected Fragment getFragment()
    {
        Fragment result = mFragmentR;
        
        if (TAB_INDEX_LEFT == mFrameType)
        {
            result = mFragmentL;
        }
        return result;
    }
    
    /**
     * Call this API for getting the Fragment Type (Current display frame)
     *
     * @return SafetyNumber<Integer> [out] Fragment Type Id
     * Range: Valid SafetyNumber<Integer> object
     * Unit: SafetyNumber<Integer>
     * Scaling: 1
     */
    protected SafetyNumber<Integer> getFragmentType()
    {
        SafetyNumber<Integer> nResult = new SafetyNumber<Integer>(
                mFrameType, -1 * mFrameType);
        
        return nResult;
    }
    
    /**
     * 
     * Function Description
     *
     * @param type
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void initLayout(int type)
    {       
        int footerButtonNumber = getFooterButtonNumber();

        switch (type)
        {
        case LayoutType.layout10:
            CommonUtils.objectCheck(onLeftTabClassName(), onRightTabClassName());

            mFragmentL = buildFragment(onLeftTabClassName());
            mFragmentR = buildFragment(onRightTabClassName());
            
            LAD0010 layout10 = new LAD0010(mContext);
            ViewGroup ll_all = (LinearLayout) mContext
                    .findViewById(R.id.lad0010_all);
            mCollectionPagerAdapter = new CollectionPageAdapter(
                    mContext.getSupportFragmentManager(), ll_all, mFragmentL,
                    mFragmentR);
            layout10.createHeader(R.drawable.plus, R.drawable.numpad,
                    mCollectionPagerAdapter);
            //layout10.setupLowerActionBar(getFooterButtonTextId(), this);
            switch (footerButtonNumber)
            {
            case 1:
                layout10.setupLowerActionBar(getFooterButtonTextId(), this);
                break;
            case 2:
                int footerLeftButtonTextId = getFooterLeftButtonTextId();
                int footerRightButtonTextId = getFooterRightButtonTextId();
                layout10.setupLowerActionBar(footerLeftButtonTextId, footerRightButtonTextId,this);
                break;
            default:
                // do nothing
                break;
            }
            mCollectionPagerAdapter.setChangeListener(this);

            break;
        case LayoutType.layout10a:
            CommonUtils.objectCheck(onLeftTabClassName(), onRightTabClassName());

            mFragmentL = buildFragment(onLeftTabClassName());
            mFragmentR = buildFragment(onRightTabClassName());
            
            LAD0010a layout10a = new LAD0010a(mContext);
            ll_all = (LinearLayout) mContext.findViewById(R.id.lad0010a_all);
            mCollectionPagerAdapter = new CollectionPageAdapter(
                    mContext.getSupportFragmentManager(), ll_all, mFragmentL,
                    mFragmentR);
            layout10a.createHeader(mRightTabIconId, mLeftTabIconId,
                    mCollectionPagerAdapter);
            //layout10a.setupLowerActionBar(R.string.txt_labeldone,R.string.txt_label_activate, this);
            switch (footerButtonNumber)
            {
            case 1:
                layout10a.setupLowerActionBar(getFooterButtonTextId(), this);
                break;
            case 2:
                int footerLeftButtonTextId = getFooterLeftButtonTextId();
                int footerRightButtonTextId = getFooterRightButtonTextId();
                layout10a.setupLowerActionBar(footerLeftButtonTextId, footerRightButtonTextId,this);
                break;
            default:
                // do nothing
                break;
            }
            
            
            break;
        case LayoutType.layout21:
            CommonUtils.objectCheck(onLeftTabClassName(), onRightTabClassName());

            mFragmentL = buildFragment(onLeftTabClassName());
            mFragmentR = buildFragment(onRightTabClassName());
            
            LAD0021 layout21 = new LAD0021(mContext);
            ll_all = (LinearLayout) mContext.findViewById(R.id.lad21_all);
            mCollectionPagerAdapter = new CollectionPageAdapter(
                    mContext.getSupportFragmentManager(), ll_all, mFragmentL,
                    mFragmentR);
            layout21.createHeader(onLeftTabIconId(), onRightTabIconId(),
                    mCollectionPagerAdapter);
            
            setupDateScroll(layout21);
            
            break;
        case LayoutType.layout21a:

            CommonUtils.objectCheck(onLeftTabClassName());

            mFragmentL = buildFragment(onLeftTabClassName());
        
            LAD0021 layout21a = new LAD0021(mContext);
            ll_all = (LinearLayout) mContext.findViewById(R.id.lad21_all);
            mCollectionPagerAdapter = new CollectionPageAdapter(
                    mContext.getSupportFragmentManager(), ll_all, mFragmentL);
            layout21a.createHeader(onLeftTabIconId(), mCollectionPagerAdapter);
            
            setupDateScroll(layout21a);
            
            break;
        default:
            throw new IllegalArgumentException("invalid layout number...");
        }
    }

    /**
     * 
     * Function Description
     *
     * @param type
     * @return void [out] Delete pre line return if exist. Parameter Description
     */
    private void setupActionBar(int type)
    {
        switch (type)
        {
        case ICON_TOTAL_NUMBER_ONE:
            // Add by Henry Tso. To support UIAutomator
            TopActionBar.setScreenId(onScreenId());

            TopActionBar.setup(mContext, onActionBarText(), onLActionBarIconId());
            
            break;
        case ICON_TOTAL_NUMBER_TWO:
            
            ImageView imageView = null;

            // Add by Henry Tso. To support UIAutomator
            TopActionBar.setScreenId(onScreenId());
            
            TopActionBar.setup(mContext, onActionBarText(), onLActionBarIconId(),
                    onRActionBarIconId());
            imageView = (ImageView) mContext.findViewById(R.id.actionbar_img2);
            imageView.setOnClickListener(new ActionButton());

            break;
            
        default:
            throw new IllegalArgumentException("invalid action bar icon number...");
        }

        
    }

    /**
     * 
     * Function Description
     *
     * @param className
     * @return
     * @return Fragment [out] Delete pre line return if exist. Parameter Description
     */
    private Fragment buildFragment(String className)
    {
        Fragment fragment = null;
        Class<?> clazz = null;
        
        CommonUtils.objectCheck(className);
        
        try
        {
            clazz = Class.forName(className);
            fragment = (Fragment) clazz.newInstance();
        }
        catch (ClassNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (InstantiationException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (IllegalAccessException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }           
        finally
        {
            
        }
        
        return fragment;
    }
    
    /**
     * 
     */
    public static interface LayoutType
    {
        int layout10 = HammingDistance.SAFETY_NUMBER_VALUE_0009;       
        int layout10a = HammingDistance.SAFETY_NUMBER_VALUE_0019;
        int layout21 = HammingDistance.SAFETY_NUMBER_VALUE_0029;
        int layout21a = HammingDistance.SAFETY_NUMBER_VALUE_0039;
    }

    /**
     * 
     */
    private class ActionButton implements OnClickListener
    {
        
        /**
         * 
         * 
         *
         * @param v
         */
        @Override
        public void onClick(View v) 
        {
            onRActionBarIconClicked(v);
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
