// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentIconList;

{
See:
https://learn.microsoft.com/en-us/windows/apps/design/style/segoe-fluent-icons-font
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes;

type

  TFluentIcons = (
  IconPathData = 0,
  //E700 - E7FF ----------------------------------------------------------------
  IconGlobalNavButton	= $e700,
  IconWifi	= $e701,
  IconBluetooth	= $e702,
  IconConnect	= $e703,
  IconInternetSharing	= $e704,
  IconVPN	= $e705,
  IconBrightness	= $e706,
  IconMapPin	= $e707,
  IconQuietHours	= $e708,
  IconAirplane	= $e709,
  IconTablet	= $e70a,
  IconQuickNote	= $e70b,
  IconRememberedDevice	= $e70c,
  IconChevronDown	= $e70d,
  IconChevronUp	= $e70e,
  IconEdit	= $e70f,
  IconAdd	= $e710,
  IconCancel	= $e711,
  IconMore	= $e712,
  IconSettings	= $e713,
  IconVideo	= $e714,
  IconMail	= $e715,
  IconPeople	= $e716,
  IconPhone	= $e717,
  IconPin	= $e718,
  IconShop	= $e719,
  IconStop	= $e71a,
  IconLink	= $e71b,
  IconFilter	= $e71c,
  IconAllApps	= $e71d,
  IconZoom	= $e71e,
  IconZoomOut	= $e71f,
  IconMicrophone= $e720,
  IconSearch	= $e721,
  IconCamera	= $e722,
  IconAttach	= $e723,
  IconSend	= $e724,
  IconSendFill	= $e725,
  IconWalkSolid	= $e726,
  IconInPrivate	= $e727,
  IconFavoriteList	= $e728,
  IconPageSolid	= $e729,
  IconForward	= $e72a,
  IconBack	= $e72b,
  IconRefresh	= $e72c,
  IconShare	= $e72d,
  IconLock	= $e72e,
  IconReportHacked	= $e730,
  IconEMI	        = $e731,
  IconFavoriteStar	= $e734,
  IconFavoriteStarFill	= $e735,
  IconReadingMode	= $e736,
  IconFavicon	= $e737,
  IconRemove	= $e738,
  IconCheckbox	= $e739,
  IconCheckboxComposite	= $e73a,
  IconCheckboxFill	= $e73b,
  IconCheckboxIndeterminate	= $e73c,
  IconCheckboxCompositeReversed	= $e73d,
  IconCheckMark	                = $e73e,
  IconBackToWindow	= $e73f,
  IconFullScreen	= $e740,
  IconResizeTouchLarger	= $e741,
  IconResizeTouchSmaller= $e742,
  IconResizeMouseSmall	= $e743,
  IconResizeMouseMedium	= $e744,
  IconResizeMouseWide	= $e745,
  IconResizeMouseTall	= $e746,
  IconResizeMouseLarge	= $e747,
  IconSwitchUser	= $e748,
  IconPrint	= $e749,
  IconUp	= $e74a,
  IconDown	= $e74b,
  IconOEM	= $e74c,
  IconDelete	= $e74d,
  IconSave	= $e74e,
  IconMute	= $e74f,
  IconBackSpaceQWERTY	= $e750,
  IconReturnKey	        = $e751,
  IconUpArrowShiftKey	= $e752,
  IconCloud	        = $e753,
  IconFlashlight	= $e754,
  IconRotationLock	= $e755,
  IconCommandPrompt	= $e756,
  IconSIPMove	= $e759,
  IconSIPUndock	= $e75a,
  IconSIPRedock	= $e75b,
  IconEraseTool	= $e75c,
  IconUnderscoreSpace	= $e75d,
  IconGripperTool	= $e75e,
  IconDialpad	= $e75f,
  IconPageLeft	= $e760,
  IconPageRight	= $e761,
  IconMultiSelect	        = $e762,
  IconKeyboardLeftHanded	= $e763,
  IconKeyboardRightHanded	= $e764,
  IconKeyboardClassic	= $e765,
  IconKeyboardSplit	= $e766,
  IconVolume	= $e767,
  IconPlay	= $e768,
  IconPause	= $e769,
  IconChevronLeft	= $e76b,
  IconChevronRight	= $e76c,
  IconInkingTool	= $e76d,
  IconEmoji2	= $e76e,
  IconGripperBarHorizontal	= $e76f,
  IconSystem	                = $e770,
  IconPersonalize	= $e771,
  IconDevices	        = $e772,
  IconSearchAndApps	= $e773,
  IconGlobe	        = $e774,
  IconTimeLanguage	= $e775,
  IconEaseOfAccess	= $e776,
  IconUpdateRestore	= $e777,
  IconHangUp	        = $e778,
  IconContactInfo	= $e779,
  IconUnpin	= $e77a,
  IconContact	= $e77b,
  IconMemo	= $e77c,
  IconIncomingCall	= $e77e,
  IconPaste	= $e77f,
  IconPhoneBook	= $e780,
  IconLEDLight	= $e781,
  IconError	= $e783,
  IconGripperBarVertical	= $e784,
  IconUnlock	= $e785,
  IconSlideshow	= $e786,
  IconCalendar	= $e787,
  IconGripperResize	= $e788,
  IconMegaphone	= $e789,
  IconTrim	= $e78a,
  IconNewWindow	= $e78b,
  IconSaveLocal	= $e78c,
  IconColor	= $e790,
  IconDataSense	= $e791,
  IconSaveAs	= $e792,
  IconLight	= $e793,
  IconAspectRatio	= $e799,
  IconDataSenseBar	= $e7a5,
  IconRedo	= $e7a6,
  IconUndo	= $e7a7,
  IconCrop	= $e7a8,
  IconOpenWith	= $e7ac,
  IconRotate	= $e7ad,
  IconRedEye	= $e7b3,
  IconSetlockScreen	= $e7b5,
  IconMapPin2	= $e7b7,
  IconPackage	= $e7b8,
  IconWarning	= $e7ba,
  IconReadingList	= $e7bc,
  IconEducation	        = $e7be,
  IconShoppingCart	= $e7bf,
  IconTrain	= $e7c0,
  IconFlag	= $e7c1,
  IconMove	= $e7c2,
  IconPage	= $e7c3,
  IconTaskView	= $e7c4,
  IconBrowsePhotos	= $e7c5,
  IconHalfStarLeft	= $e7c6,
  IconHalfStarRight	= $e7c7,
  IconRecord	        = $e7c8,
  IconTouchPointer	= $e7c9,
  IconLangJPN	= $e7de,
  IconFerry	= $e7e3,
  IconHighlight	= $e7e6,
  IconActionCenterNotification	= $e7e7,
  IconPowerButton	        = $e7e8,
  IconResizeTouchNarrower	= $e7ea,
  IconResizeTouchShorter	= $e7eb,
  IconDrivingMode	= $e7ec,
  IconRingerSilent	= $e7ed,
  IconOtherUser	= $e7ee,
  IconAdmin	= $e7ef,
  IconCC	= $e7f0,
  IconSDCard	= $e7f1,
  IconCallForwarding	        = $e7f2,
  IconSettingsDisplaySound	= $e7f3,
  IconTVMonitor	= $e7f4,
  IconSpeakers	= $e7f5,
  IconHeadphone	= $e7f6,
  IconDeviceLaptopPic	= $e7f7,
  IconDeviceLaptopNoPic	= $e7f8,
  IconDeviceMonitorRightPic	= $e7f9,
  IconDeviceMonitorLeftPic	= $e7fa,
  IconDeviceMonitorNoPic	= $e7fb,
  IconGame	                = $e7fc,
  IconHorizontalTabKey	        = $e7fd,

  //E800 - E8FF ----------------------------------------------------------------
  IconStreetsideSplitMinimize	= $e802,
  IconStreetsideSplitExpand	= $e803,
  IconCar	= $e804,
  IconWalk	= $e805,
  IconBus	= $e806,
  IconTiltUp	= $e809,
  IconTiltDown	= $e80a,
  IconCallControl	= $e80b,
  IconRotateMapRight	= $e80c,
  IconRotateMapLeft	= $e80d,
  IconHome	= $e80f,
  IconParkingLocation	= $e811,
  IconMapCompassTop	= $e812,
  IconMapCompassBottom	= $e813,
  IconIncidentTriangle	= $e814,
  IconTouch	= $e815,
  IconMapDirections	= $e816,
  IconStartPoint	= $e819,
  IconStopPoint	= $e81a,
  IconEndPoint	= $e81b,
  IconHistory	= $e81c,
  IconLocation	= $e81d,
  IconMapLayers	= $e81e,
  IconAccident	= $e81f,
  IconWork	= $e821,
  IconConstruction	= $e822,
  IconRecent	= $e823,
  IconBank	= $e825,
  IconDownloadMap	= $e826,
  IconInkingToolFill2	= $e829,
  IconHighlightFill2	= $e82a,
  IconEraseToolFill	= $e82b,
  IconEraseToolFill2	= $e82c,
  IconDictionary	= $e82d,
  IconDictionaryAdd	= $e82e,
  IconToolTip	= $e82f,
  IconChromeBack	= $e830,
  IconProvisioningPackage	= $e835,
  IconAddRemoteDevice	= $e836,
  IconFolderOpen	= $e838,
  IconEthernet	= $e839,
  IconShareBroadband	= $e83a,
  IconDirectAccess	= $e83b,
  IconDialUp	= $e83c,
  IconDefenderApp	= $e83d,
  IconBatteryCharging9	= $e83e,
  IconBattery10	= $e83f,
  IconPinned	= $e840,
  IconPinFill	= $e841,
  IconPinnedFill	= $e842,
  IconPeriodKey	= $e843,
  IconPuncKey	= $e844,
  IconRevToggleKey	= $e845,
  IconRightArrowKeyTime1	= $e846,
  IconRightArrowKeyTime2	= $e847,
  IconLeftQuote	= $e848,
  IconRightQuote	= $e849,
  IconDownShiftKey	= $e84a,
  IconUpShiftKey	= $e84b,
  IconPuncKey0	= $e84c,
  IconPuncKeyLeftBottom	= $e84d,
  IconRightArrowKeyTime3	= $e84e,
  IconRightArrowKeyTime4	= $e84f,
  IconBattery0	= $e850,
  IconBattery1	= $e851,
  IconBattery2	= $e852,
  IconBattery3	= $e853,
  IconBattery4	= $e854,
  IconBattery5	= $e855,
  IconBattery6	= $e856,
  IconBattery7	= $e857,
  IconBattery8	= $e858,
  IconBattery9	= $e859,
  IconBatteryCharging0	= $e85a,
  IconBatteryCharging1	= $e85b,
  IconBatteryCharging2	= $e85c,
  IconBatteryCharging3	= $e85d,
  IconBatteryCharging4	= $e85e,
  IconBatteryCharging5	= $e85f,
  IconBatteryCharging6	= $e860,
  IconBatteryCharging7	= $e861,
  IconBatteryCharging8	= $e862,
  IconBatterySaver0	= $e863,
  IconBatterySaver1	= $e864,
  IconBatterySaver2	= $e865,
  IconBatterySaver3	= $e866,
  IconBatterySaver4	= $e867,
  IconBatterySaver5	= $e868,
  IconBatterySaver6	= $e869,
  IconBatterySaver7	= $e86a,
  IconBatterySaver8	= $e86b,
  IconSignalBars1	= $e86c,
  IconSignalBars2	= $e86d,
  IconSignalBars3	= $e86e,
  IconSignalBars4	= $e86f,
  IconSignalBars5	= $e870,
  IconSignalNotConnected	= $e871,
  IconWifi1	= $e872,
  IconWifi2	= $e873,
  IconWifi3	= $e874,
  IconMobSIMLock	= $e875,
  IconMobSIMMissing	= $e876,
  IconVibrate	= $e877,
  IconRoamingInternational	= $e878,
  IconRoamingDomestic	= $e879,
  IconCallForwardInternational	= $e87a,
  IconCallForwardRoaming	= $e87b,
  IconJpnRomanji	= $e87c,
  IconJpnRomanjiLock	= $e87d,
  IconJpnRomanjiShift	= $e87e,
  IconJpnRomanjiShiftLock	= $e87f,
  IconStatusDataTransfer	= $e880,
  IconStatusDataTransferVPN	= $e881,
  IconStatusDualSIM2	= $e882,
  IconStatusDualSIM2VPN	= $e883,
  IconStatusDualSIM1	= $e884,
  IconStatusDualSIM1VPN	= $e885,
  IconStatusSGLTE	= $e886,
  IconStatusSGLTECell	= $e887,
  IconStatusSGLTEDataVPN	= $e888,
  IconStatusVPN	= $e889,
  IconWifiHotspot	= $e88a,
  IconLanguageKor	= $e88b,
  IconLanguageCht	= $e88c,
  IconLanguageChs	= $e88d,
  IconUSB	= $e88e,
  IconInkingToolFill	= $e88f,
  IconView	= $e890,
  IconHighlightFill	= $e891,
  IconPrevious	= $e892,
  IconNext	= $e893,
  IconClear	= $e894,
  IconSync	= $e895,
  IconDownload	= $e896,
  IconHelp	= $e897,
  IconUpload	= $e898,
  IconEmoji	= $e899,
  IconTwoPage	= $e89a,
  IconLeaveChat	= $e89b,
  IconMailForward	= $e89c,
  IconRotateCamera	= $e89e,
  IconClosePane	= $e89f,
  IconOpenPane	= $e8a0,
  IconPreviewLink	= $e8a1,
  IconAttachCamera	= $e8a2,
  IconZoomIn	= $e8a3,
  IconBookmarks	= $e8a4,
  IconDocument	= $e8a5,
  IconProtectedDocument	= $e8a6,
  IconOpenInNewWindow	= $e8a7,
  IconMailFill	= $e8a8,
  IconViewAll	= $e8a9,
  IconVideoChat	= $e8aa,
  IconSwitch	= $e8ab,
  IconRename	= $e8ac,
  IconGo	= $e8ad,
  IconSurfaceHub	= $e8ae,
  IconRemote	= $e8af,
  IconClick	= $e8b0,
  IconShuffle	= $e8b1,
  IconMovies	= $e8b2,
  IconSelectAll	= $e8b3,
  IconOrientation	= $e8b4,
  IconImport	= $e8b5,
  IconImportAll	= $e8b6,
  IconFolder	= $e8b7,
  IconWebcam	= $e8b8,
  IconPicture	= $e8b9,
  IconCaption	= $e8ba,
  IconChromeClose	= $e8bb,
  IconShowResults	= $e8bc,
  IconMessage	= $e8bd,
  IconLeaf	= $e8be,
  IconCalendarDay	= $e8bf,
  IconCalendarWeek	= $e8c0,
  IconCharacters	= $e8c1,
  IconMailReplyAll	= $e8c2,
  IconRead	= $e8c3,
  IconShowBcc	= $e8c4,
  IconHideBcc	= $e8c5,
  IconCut	= $e8c6,
  IconPaymentCard	= $e8c7,
  IconCopy	= $e8c8,
  IconImportant	= $e8c9,
  IconMailReply	= $e8ca,
  IconSort	= $e8cb,
  IconMobileTablet	= $e8cc,
  IconDisconnectDrive	= $e8cd,
  IconMapDrive	= $e8ce,
  IconContactPresence	= $e8cf,
  IconPriority	= $e8d0,
  IconGotoToday	= $e8d1,
  IconFont	= $e8d2,
  IconFontColor	= $e8d3,
  IconContact2	= $e8d4,
  IconFolderFill	= $e8d5,
  IconAudio	= $e8d6,
  IconPermissions	= $e8d7,
  IconDisableUpdates	= $e8d8,
  IconUnfavorite	= $e8d9,
  IconOpenLocal	= $e8da,
  IconItalic	= $e8db,
  IconUnderline	= $e8dc,
  IconBold	= $e8dd,
  IconMoveToFolder	= $e8de,
  IconLikeDislike	= $e8df,
  IconDislike	= $e8e0,
  IconLike	= $e8e1,
  IconAlignRight	= $e8e2,
  IconAlignCenter	= $e8e3,
  IconAlignLeft	= $e8e4,
  IconOpenFile	= $e8e5,
  IconClearSelection	= $e8e6,
  IconFontDecrease	= $e8e7,
  IconFontIncrease	= $e8e8,
  IconFontSize	= $e8e9,
  IconCellPhone	= $e8ea,
  IconReshare	= $e8eb,
  IconTag	= $e8ec,
  IconRepeatOne	= $e8ed,
  IconRepeatAll	= $e8ee,
  IconCalculator	= $e8ef,
  IconDirections	= $e8f0,
  IconLibrary	= $e8f1,
  IconChatBubbles	= $e8f2,
  IconPostUpdate	= $e8f3,
  IconNewFolder	= $e8f4,
  IconCalendarReply	= $e8f5,
  IconUnsyncFolder	= $e8f6,
  IconSyncFolder	= $e8f7,
  IconBlockContact	= $e8f8,
  IconSwitchApps	= $e8f9,
  IconAddFriend	= $e8fa,
  IconAccept	= $e8fb,
  IconGoToStart	= $e8fc,
  IconBulletedList	= $e8fd,
  IconScan	= $e8fe,
  IconPreview	= $e8ff,

  // E900 - E9FF ---------------------------------------------------------------
  IconGroup	= $e902,
  IconZeroBars	= $e904,
  IconOneBar	= $e905,
  IconTwoBars	= $e906,
  IconThreeBars	= $e907,
  IconFourBars	= $e908,
  IconWorld	= $e909,
  IconComment	= $e90a,
  IconMusicInfo	= $e90b,
  IconDockLeft	= $e90c,
  IconDockRight	= $e90d,
  IconDockBottom= $e90e,
  IconRepair	= $e90f,
  IconAccounts	= $e910,
  IconDullSound	= $e911,
  IconManage	= $e912,
  IconStreet	= $e913,
  IconPrinter3D	= $e914,
  IconRadioBullet	= $e915,
  IconStopwatch	= $e916,
  IconPhoto	= $e91b,
  IconActionCenter	= $e91c,
  IconFullCircleMask	= $e91f,
  IconChromeMinimize	= $e921,
  IconChromeMaximize	= $e922,
  IconChromeRestore	= $e923,
  IconAnnotation	= $e924,
  IconBackSpaceQWERTYSm	= $e925,
  IconBackSpaceQWERTYMd	= $e926,
  IconSwipe	= $e927,
  IconFingerprint	= $e928,
  IconHandwriting	= $e929,
  IconChromeBackToWindow= $e92c,
  IconChromeFullScreen	= $e92d,
  IconKeyboardStandard	= $e92e,
  IconKeyboardDismiss	= $e92f,
  IconCompleted	= $e930,
  IconChromeAnnotate	= $e931,
  IconLabel	= $e932,
  IconIBeam	= $e933,
  IconIBeamOutline	= $e934,
  IconFlickDown	= $e935,
  IconFlickUp	= $e936,
  IconFlickLeft	= $e937,
  IconFlickRight	= $e938,
  IconFeedbackApp	= $e939,
  IconMusicAlbum	= $e93c,
  IconStreaming	= $e93e,
  IconCode	= $e943,
  IconReturnToWindow	= $e944,
  IconLightningBolt	= $e945,
  IconInfo	= $e946,
  IconCalculatorMultiply	= $e947,
  IconCalculatorAddition	= $e948,
  IconCalculatorSubtract	= $e949,
  IconCalculatorDivide	= $e94a,
  IconCalculatorSquareroot	= $e94b,
  IconCalculatorPercentage	= $e94c,
  IconCalculatorNegate	= $e94d,
  IconCalculatorEqualTo	= $e94e,
  IconCalculatorBackspace	= $e94f,
  IconComponent	= $e950,
  IconDMC	= $e951,
  IconDock	= $e952,
  IconMultimediaDMS	= $e953,
  IconMultimediaDVR	= $e954,
  IconMultimediaPMP	= $e955,
  IconPrintfaxPrinterFile	= $e956,
  IconSensor	= $e957,
  IconStorageOptical	= $e958,
  IconCommunications	= $e95a,
  IconHeadset	= $e95b,
  IconProjector	= $e95d,
  IconHealth	= $e95e,
  IconWire	= $e95f,
  IconWebcam2	= $e960,
  IconInput	= $e961,
  IconMouse	= $e962,
  IconSmartcard	= $e963,
  IconSmartcardVirtual	= $e964,
  IconMediaStorageTower	= $e965,
  IconReturnKeySm	= $e966,
  IconGameConsole	= $e967,
  IconNetwork	= $e968,
  IconStorageNetworkWireless	= $e969,
  IconStorageTape	= $e96a,
  IconChevronUpSmall	= $e96d,
  IconChevronDownSmall	= $e96e,
  IconChevronLeftSmall	= $e96f,
  IconChevronRightSmall	= $e970,
  IconChevronUpMed	= $e971,
  IconChevronDownMed	= $e972,
  IconChevronLeftMed	= $e973,
  IconChevronRightMed	= $e974,
  IconDevices2	        = $e975,
  IconExpandTile	= $e976,
  IconPC1	        = $e977,
  IconPresenceChicklet	= $e978,
  IconPresenceChickletVideo	= $e979,
  IconReply	= $e97a,
  IconSetTile	= $e97b,
  IconIconType	= $e97c,
  IconKorean	= $e97d,
  IconHalfAlpha	= $e97e,
  IconFullAlpha	= $e97f,
  IconKey12On	= $e980,
  IconChineseChangjie	= $e981,
  IconQWERTYOn	= $e982,
  IconQWERTYOff	= $e983,
  IconChineseQuick	= $e984,
  IconJapanese	= $e985,
  IconFullHiragana	= $e986,
  IconFullKatakana	= $e987,
  IconHalfKatakana	= $e988,
  IconChineseBoPoMoFo	= $e989,
  IconChinesePinyin	= $e98a,
  IconConstructionCone	= $e98f,
  IconXboxOneConsole	= $e990,
  IconVolume0	= $e992,
  IconVolume1	= $e993,
  IconVolume2	= $e994,
  IconVolume3	= $e995,
  IconBatteryUnknown	= $e996,
  IconWifiAttentionOverlay	= $e998,
  IconRobot	= $e99a,
  IconTapAndSend= $e9a1,
  IconFitPage	= $e9a6,
  IconPasswordKeyShow	= $e9a8,
  IconPasswordKeyHide	= $e9a9,
  IconBidiLtr	= $e9aa,
  IconBidiRtl	= $e9ab,
  IconForwardSm	= $e9ac,
  IconCommaKey	= $e9ad,
  IconDashKey	= $e9ae,
  IconDullSoundKey	= $e9af,
  IconHalfDullSound	= $e9b0,
  IconRightDoubleQuote	= $e9b1,
  IconLeftDoubleQuote	= $e9b2,
  IconPuncKeyRightBottom= $e9b3,
  IconPuncKey1	= $e9b4,
  IconPuncKey2	= $e9b5,
  IconPuncKey3	= $e9b6,
  IconPuncKey4	= $e9b7,
  IconPuncKey5	= $e9b8,
  IconPuncKey6	= $e9b9,
  IconPuncKey9	= $e9ba,
  IconPuncKey7	= $e9bb,
  IconPuncKey8	= $e9bc,
  IconFrigid	= $e9ca,
  IconUnknown	= $e9ce,
  IconAreaChart	= $e9d2,
  IconCheckList	= $e9d5,
  IconDiagnostic= $e9d9,
  IconEqualizer	= $e9e9,
  IconProcess	= $e9f3,
  IconProcessing= $e9f5,
  IconReportDocument= $e9f9,

  IconDateTime = $ec92
 );

  const
  IconUserPath = 'E18,2,28,28S M52,38A46,38,12,12,0,90 C6,44,58,68,6,68A6,38,12,12,-90,0ZS';

  RightCircleBadgeClip = 'A24,24,44,44,-170,80L64,0L0,0L0,64Z';

  BadgeForwardPath = 'E28,28,36,36F1M38,46L54,46M46,38L54,46L46,54S2';

  BadgeReplyPath = 'E28,28,36,36F1'
    +'M43,38L35,46L43,54M35,46A43,46,15,15,0,90S2';

  BadgeReplyAllPath = 'E28,28,36,36F1'
    +'M40,38L32,46L40,54'
    +'M50,38L42,46L50,54M42,46A43,46,15,15,0,90S2';

  BadgeAddPath = 'E28,28,36,36F1'
    +'M46,38L46,54M38,46L54,46S2';

  BadgeSwitchPath = 'M38,46L30,54L38,62M30,54L62,54M54,46L62,54L54,62S1';

  BadgeSwitchClip = 'M32,64L15,47L64,40L64,0L0,0L0,64Z';


  //e700 -----------------------------------------------------------------------
  //e700 IconGlobalNavButton
  IconGlobalNavButtonPath = 'M2,10L62,10M2,30L62,30M2,50L62,50S';
  IconGlobalNavButtonName = 'GlobalNavButton';

  //e701 IconWiFi
  IconWiFiPath = 'E27,43,10,10FA-8,10,80,92,-48,48SA4,22,56,70,-48,48SA15,34,34,46,-48,48S';
  IconWiFiName = 'WiFi';

  //e702 IconBluetooth
  IconBluetoothPath = 'M16,18L48,46L32,62L32,2L48,18L16,48S';
  IconBluetoothName = 'Bluetooth';

  //e706 IconBrightness
  IconBrightnessPath = 'E18,18,28,28SM32,2L32,6M62,32L58,32M32,62L32,58M2,32'
    +'L6,32M54,10L50,14M54,54L50,50M10,54L14,50M10,10L14,14S1';
  IconBrightnessName = 'Brightness';

  //e70d IconChevronDown
  IconChevronDownPath = 'M10,22L32,44L54,22S';
  IconChevronDownName = 'ChevronDown';

  //e70e IconChevronUp
  IconChevronUpPath = 'M10,42L32,20L54,42S';
  IconChevronUpName = 'ChevronUp';

  //e70f IconEdit
  IconEditPath = 'M3,61L7,45 A45,2,17,17,-45,135 L19,57ZM44,8L56,20S';
  IconEditName = 'Edit';

  //e710 -----------------------------------------------------------------------
  IconAddPath = 'M30,6L30,54M6,30L54,30S';
  IconAddName = 'Add';

  IconCancelPath = 'M10,10L50,50M10,50,L50,10S';
  IconCancelName = 'Cancel';

  IconMorePath = 'E7,27,10,10E27,27,10,10E47,27,10,10F';
  IconMoreName = 'More';

  IconSettingsPath = 'M26,2L38,2C55,12,41,16,41,16L61,22C61,42,50,32,50,32'
    +'L55,52C38,62,41,48,41,48L26,62C9,52,23,48,23,48L3,42C3,22,14,32,14,32'
    +'L9,12C26,2,23,16,23,16E22,22,20,20S';
  IconSettingsName = 'Settings';

  IconVideoPath = 'R2,10,44,44,8M46,26A58,16,4,4,-37,90A58,44,4,4,90,217L46,38S';
  IconVideoName = 'Video';

  IconMailPath = 'R2,10,60,48,8M62,20,L32,38,L2,20S';
  IconMailName = 'Mail';

  IconPeoplePath = 'M45,30C42,58,52,40,51,47L64,58L64,30ZPM46,34A54,34,8,8,0,90'
    +'A32,26,30,28,90,180L40,54S1UA2,34,10,10,0,-90A2,24,36,34,-90,-180'
    +'A6,24,36,34,180,90A32,34,10,10,90,0ZE10,2,24,24SE42,10,16,16S1';
  IconPeopleName = 'People';

  IconPhonePath = 'M18,3.4C50,58.6,-6.4,11,31,77C51,46,56,54,54,50'
    +'C39,41,46,40,47,38C27.4,35.4,33.6,43.2,31.4,42.8C29,21,22.6,26,23.2,25.6'
    +'C29.4,8,33,17,32.6,14.4C18,3.4,28.2,4.8,26,-0.4S';
  IconPhoneName = 'Phone';

  // e718 IconPin
  IconPinPath = 'T0,0,64,64PM6,30A19,15,8,8,160,115A33,3,8,8,-60,45'
    +'A53,23,8,8,45,150A40,36,8,8,-25,-75L35,58ZM0,64L20,44SU';
  IconPinName = 'Pin';

  //e71c IconFilter
  IconFilterPath ='M26,54A14,32,12,12,90,45A2,6,8,8-135,0A54,6,8,8,0,135A38,32,12,12,-45,-90L38,62ZS';
  IconFilterName ='Filter';

  // e71e IconZoom
  IconZoomPath ='E6,6,40,40M58,58L41,41S';
  IconZoomName ='Zoom';

  IconZoomOutPath = 'M16,26L36,26' + IconZoomPath;
  IconZoomOutName = 'ZoomOut';

  //e720 -----------------------------------------------------------------------
  //e721 Search
  IconSearchPath = 'E18,6,40,40M6,58L23,41S';
  IconSearchName = 'Search';

  //e722 IconCamera
  IconCameraPath = 'M46,10A50,10,12,12,0,90A50,46,12,12,90,180'
    +'A2,46,12,12,-180,-90A2,10,12,12,-90,0L18,10'
    +'C25,2,20,8,20,2L39,2C46,10,44,2,44,8E18,18,28,28S';
  IconCameraName = 'Camera';

  //e723 IconAttach
  IconAttachPath = 'M6,34A26,6,28,28,-45,135A10,42,16,16,135,315L34,22.5S';
  IconAttachName = 'Attach';

  //e724 IconSend
  IconSendPath = 'M10,32L2,4L62,32L2,60ZL38,32S';
  IconSendName = 'Send';

  //e72a Forward
  IconForwardPath = 'M2,30L62,30M36,2L62,30L36,58S';
  IconForwardName = 'Forward';

  //e72b Back
  IconBackPath = 'M2,30L62,30M28,2L2,30L28,58S';
  IconBackName = 'Back';

  //e72c Refresh
  IconRefreshPath = 'A2,2,60,60,90,420M58,2L58,18L42,18S';
  IconRefreshName = 'Refresh';

  //e72d Share
  IconSharePath = 'T0,0,44,64PM16,42C44,32,24,36,31,32M16,42C44,16,19,26,27,16S1U'
    +'M42,15L42,5L60,24L42,43L42,33S1M54,46,A42,46,12,12,90,180'
    +'A6,46,12,12,180,270A6,10,12,12,270,360L26,10S';
  IconShareName = 'Share';

  //e730 -----------------------------------------------------------------------
  //e734 FavoriteStar
  IconFavoriteStarPath = 'M32,3.5L41.4,22,L62,25.3L47.3,40L50.5,60.5L32,51'
    +'L13.5,60.5L16.7,40L2,25.3L22.6,22ZS';
  IconFavoriteStarName = 'FavoriteStar';

  //e735 FavoriteStarFill
  IconFavoriteStarFillPath = IconFavoriteStarPath+'0,1F';
  IconFavoriteStarFillName = 'FavoriteStarFill';

  //e739 IconCheckbox
  IconCheckboxPath = 'R2,2,60,60,6S';
  IconCheckboxName = 'Checkbox';

  //e73a IconCheckboxComposite
  IconCheckboxCompositePath = IconCheckboxPath+'M14,30L26,42L50,18S1';
  IconCheckboxCompositeName = 'CheckboxComposite';

  //e740 -----------------------------------------------------------------------


  //e748 IconSwitchUser
  IconSwitchUserPath =  BadgeSwitchClip+'P'
    + IconUserPath
    +'U'+BadgeSwitchPath;
  IconSwitchUserName = 'SwitchUser';

  //e749 IconPrint
  IconPrintPath = 'R14,34,36,24,5M14,46A2,38,8,8,180,270A2,14,16,16,-90,0'
    +'A46,14,16,16,0,90A54,38,8,8,90,180L50,46M14,14A14,6,8,8,-90,0'
    +'A42,6,8,8,0,90L50,14S';
  IconPrintName = 'Print';

  //e74c IconOEM
  IconOEMPath =  'M6,34A46,34,8,8,0,90A46,50,8,8,90,180A6,50,8,8,-180,-90'
    +'A6,10,8,8,-90,0A22,10,8,8,0,90L30,58S'
    +'A50,16,8,8,45,135   A40,26,8,8,135,215'
    +'A30,16,8,8,-135,-45   A40,6,8,8,-45,45'
    +'ZS1';
  IconOEMName = 'OEM';

  //e74d IconDelete
  IconDeletePath = 'A24,2,16,16,-90,90M8,10A13,50,12,12,-97,-180'
    +'A39,50,12,12,180,97L56,10M4,10L60,10S'
    +'M26,26L26,46M38,26L38,46S1';
  IconDeleteName = 'Delete';

  //e74e IconSave
  IconSavePath = 'A6,46,12,12,-180,-90A6,6,12,12,-90,0A38,6,12,12,0,45'
    +'A46,14,12,12,45,90A46,46,12,12,90,180ZM14,58A14,34,8,8,-90,0'
    +'A42,34,8,8,0,90L50,58M18,6A18,14,8,8,-90,-180A34,14,8,8,180,90L42,6S';
  IconSaveName = 'Save';

  //e753 IconCloud
  IconCloudPath = 'A15,10,34,34,-87,87A39,27,23,23,0,180A2,27,23,23,-180,0ZS';
  IconCloudName = 'Cloud';

  //e760 IconPageLeft
  IconPageLeftPath = 'E4,4,56,56M36,20L24,32L36,44S';
  IconPageLeftName = 'PageLeft';

  //e761 IconPageRight
  IconPageRightPath = 'E4,4,56,56M28,20L40,32L28,44S';
  IconPageRightName = 'PageRight';

  //e774 IconGlobe
  IconGlobePath = 'E2,2,60,60E18,2,28,60M4,22L60,22M4,42L60,42S';
  IconGlobeName = 'Globe';

  //e77b IconContact
  IconContactPath = IconUserPath;
  IconContactName = 'Contact';

  // e783 IconError
  IconErrorPath = 'E4,4,56,56M32,20L32,36SE29,41,6,6F';
  IconErrorName = 'Error';

  //e793 IconLight
  IconLightPath = 'A18,18,28,28,-180,0ZF'+IconBrightnessPath;
  IconLightName = 'Light';

  // e7b3 IconRedEye
  IconRedEyePath = 'E20,26,24,24A1,14,62,74,-70,70S';
  IconRedEyeName = 'RedEye';

  //e7e8 IconPowerButton
  IconPowerButtonPath = 'A4,6,56,56,30,330SM32,2L32,26S1';
  IconPowerButtonName = 'PowerButton';

  // E800 - E8FF --------------------------------------------------------------
  //e80f IconHome
  IconHomePath = 'A27,2,10,10,-45,45A51,24,7,7,45,90A46,46,12,12,90,180'
    +'A38,54,4,4,-180,-90A34,38,4,4,90,0A26,38,4,4,0,-90A22,54,4,4,90,180'
    +'A6,46,12,12,-180,-90A6,24,7,7,-90,-45ZS';
  IconHomeName = 'Home';

  //e81e IconMapLayers
  IconMapLayersPath = 'M6,22L32,6L58,22L32,38ZS'
    +'M6,34L34,50L58,34M6,46L34,62L58,46S1';
  IconMapLayersName = 'MapLayers';


  // e838 IconFolderOpen
  IconFolderOpenPath = 'M5,57A12,22,12,12,-72,0A52,22,10,10,0,94'
    +'A46,46,12,12,108,180A2,46,12,12,-180,-90A2,6,12,12,-90,0A17,6,12,12,0,54'
    +'L32,14A46,14,12,12,0,90S';
  IconFolderOpenName = 'FolderOpen';

  // e88e IconUSB
  IconUSBPath = 'R14,18,36,44,6M22,17L22,2L42,2L42,17S';
  IconUSBName = 'USB';

  //e890 IconView
  IconViewPath = IconRedEyePath;
  IconViewName = 'View';

  //e895 IconSync
  IconSyncPath = 'A2,2,60,60,-90,60M58,18L58,2M58,18L42,18SA2,2,60,60,90,240M6,46L6,62M6,46L22,46S';
  IconSyncName = 'Sync';

  //e896 IconDownload
  IconDownloadPath = 'M32,2L32,54M14,36L32,54L50,36SM12,62L52,62S1';
  IconDownloadName = 'Download';

  //e897 IconHelp
  IconHelpPath = 'A18,6,28,28,-90,90C32,46,46,32,32,32SE29,54,6,6F1';
  IconHelpName = 'Help';

  //e898 IconUpload
  IconUploadPath = 'M32,62L32,10M14,28L32,10L50,28SM12,2L52,2S1';
  IconUploadName = 'Upload';

  //e89c IconMailForward
  IconMailForwardPath = RightCircleBadgeClip+'P'+IconMailPath+'U'+BadgeForwardPath;
  IconMailForwardName = 'MailForward';

  //e8a3 IconZoomIn
  IconZoomInPath = 'M26,16L26,36' + IconZoomOutPath;
  IconZoomInName = 'ZoomIn';

  //e8a5 IconDocument
  IconDocumentPath = 'M34,4A34,10,12,12,-90,-180L52,22M10,14A10,2,12,12,-90,0'
    +'A25,2,12,12,0,45A42,18,12,12,45,90A42,50,12,12,90,180'
    +'A10,50,12,12,-180,-90ZS';
  IconDocumentName = 'Document';

  //e8ab IconSwitch
  IconSwitchPath = 'M10,18L54,18M38,2L54,18L38,34SM54,46L10,46M26,30L10,46L26,62S1';
  IconSwitchName = 'Switch';

  //e8b7 IconFolder
  IconFolderPath = 'M2,18L25,18C32,14,28,17,29,16A50,14,12,12,0,90'
    +'A50,46,12,12,90,180A2,46,12,12,-180,-90A2,6,12,12,-90,0L23,6C32,14,28,6,29,11S';
  IconFolderName = 'Folder';


  //e8b9 IconPicture
  IconPicturePath = 'M56,0L64,0L64,64L0,64L0,56L32,56L56,32ZPR10,10,48,48,9SU'
    +'M8,48A22,30,12,12,-45,45L48,48S1R6,6,44,44,6SE34,14,8,8F1';
  IconPictureName = 'Picture';

  //e8bb ChromeClose
  IconChromeClosePath = 'M2,2L62,62M2,62L62,2S';
  IconChromeCloseName = 'ChromeClose';

  //e8bd IconMessage
  IconMessagePath = 'M2,62A2,2,60,60,-120,210ZM22,26L42,26M22,38L34,38S';
  IconMessageName = 'Message';

  //e8be IconLeaf
  IconLeafPath = 'A10,10,44,44,45,315A27,2,10,10,-45,45ZM32,30L32,62S';
  IconLeafName = 'Leaf';

  //e8c1 Characters
  IconCharactersPath = 'M6,58L20,26L34,58M10,50L30,50M26,12L26,6L58,6L58,12'
    +'M42,2L42,6M32,14L48,14L42,20L42,31C37,34,42,34,42,34M28,22L56,22S';
  IconCharactersName = 'Characters';

  //e8c2 IconMailReplyAll
  IconMailReplyAllPath = RightCircleBadgeClip+'P'+IconMailPath+'U'+BadgeReplyAllPath;
  IconMailReplyAllName = 'MailReplyAll';

  //e8c3 MailRead
  IconMailReadPath = 'A2,42,16,16,-180,-90A2,20,16,16,-90,-30L32,6A46,20,16,16,30,90A46,42,16,16,90,180ZM60,25L32,40L4,25S';
  IconMailReadName = 'MailRead';

  //e8c6 Cut
  IconCutPath = 'M14,2L41,44,M32,30L23,44SM19,0L61,64L62,0ZPM23,43,L50,2SU'
    +'E38,42,20,20E6,42,20,20S1';
  IconCutName = 'Cut';

  //e8ca IconMailReply
  IconMailReplyPath = RightCircleBadgeClip+'P'+IconMailPath+'U'+BadgeReplyPath;
  IconMailReplyName = 'MailReply';

  //e8cb IconSort
  IconSortPath = 'M18,10L18,54M2,26L18,10L34,26S'
    +'M46,10L46,54M30,38L46,54L62,38S1';
  IconSortName = 'Sort';


  //e8d2 IconFont
  IconFontPath = 'M0.0L39,0L15,64L0,64ZPM2,38L16,2L30,38M7,27L25,27SU'
    +'M22,62L40,14L58,62M29,46L51,46M18,62L26,62M54,62L62,62S';
  IconFontName = 'Font';

  //e8d3 IconFontColor
  IconFontColorPath = 'M20,32L32,2L44,32M25,22L39,22SR6,42,52,20,4S1';
  IconFontColorName = 'FontColor';

  //e8e9 IconFontSize
  IconFontSizePath = 'M2,54L14,25L26,54M8,42L20,42S1M26,54L44,7L62,54M33,38L55,38S';
  IconFontSizeName = 'FontSize';

  //e8ec IconTag
  IconTagPath = 'A5,28,12,12,-135,-45A30,2,12,12,-45,0A50,2,12,12,0,90'
  +'A50,22,12,12,90,135A24,48,12,12,135,215ZSE44,12,8,8F1';
  IconTagName = 'Tag';

  //e8ef IconCalculator
  IconCalculatorPath = 'R10,2,44,60,8R18,10,28,12,4S'
    +'E16,32,8,8     E28,32,8,8     E40,32,8,8'
    +'E16,44,8,8     E28,44,8,8     E40,44,8,8F1';
  IconCalculatorName = 'Calculator';

  //e8f2 IconChatBubbles
  IconChatBubblesPath = 'M0,0L0,64,L64,64,L64,0ZE-4,-4,56,56PA18,18,44,44,-210,120'
    +'L62,62ZSUA2,2,44,44-120,210L2,46ZM18,20L30,20M18,28L27,28S';
  IconChatBubblesName = 'ChatBubbles';

  //e8f4 IconNewFolder
  IconNewFolderPath =  RightCircleBadgeClip+'P'+IconFolderPath+'U'
    +BadgeAddPath;
  IconNewFolderName = 'NewFolder';

  //e8fa IconAddFriend
  IconAddFriendPath =  RightCircleBadgeClip+'P'+IconContactPath+'U'
    +BadgeAddPath;
  IconAddFriendName = 'AddFriend';

  //e8fb IconAccept
  IconAcceptPath =  'M2,30L22,50L62,10S';
  IconAcceptName = 'Accept';

  //ec92 IconDateTime
  IconDateTimePath = RightCircleBadgeClip+'PR6,6,52,52,6M6,18L58,18SUE28,28,36,36F1M46,38L46,46L54,46S2';
  IconDateTimeName = 'DateTime';


const
  IconPathsE7: array of TIdentMapEntry = (
  //e700
    (Value: ord(IconGlobalNavButton); Name: IconGlobalNavButtonPath),      //e700
    (Value: ord(IconWiFi);   Name: IconWiFiPath),               //e701
    (Value: ord(IconBluetooth);   Name: IconBluetoothPath),     //e702
    //(Value: ;   Name: ),   //e703
    //(Value: ;   Name: ),   //e704
    //(Value: ;   Name: ),   //e705
    (Value: ord(IconBrightness);   Name: IconBrightnessPath),   //e706
    //(Value: ;   Name: ),   //e707
    //(Value: ;   Name: ),   //e708
    //(Value: ;   Name: ),   //e709
    //(Value: ;   Name: ),   //e70a
    //(Value: ;   Name: ),   //e70b
    //(Value: ;   Name: ),   //e70c
    (Value: ord(IconChevronDown); Name: IconChevronDownPath),   //e70d
    (Value: ord(IconChevronUp);   Name: IconChevronUpPath),     //e70e
    (Value: ord(IconEdit);   Name: IconEditPath),               //e70f
  //e710
    (Value: ord(IconAdd);   Name: IconAddPath),                 //e710
    (Value: ord(IconCancel);   Name: IconCancelPath),           //e711
    (Value: ord(IconMore);   Name: IconMorePath),               //e712
    (Value: ord(IconSettings);   Name: IconSettingsPath),       //e713
    (Value: ord(IconVideo);   Name: IconVideoPath),             //e714
    (Value: ord(IconMail);   Name: IconMailPath),               //e715
    (Value: ord(IconPeople);   Name: IconPeoplePath),           //e716
    (Value: ord(IconPhone);   Name: IconPhonePath),             //e717
    (Value: ord(IconPin);   Name: IconPinPath),                 //e718
    //(Value: ;   Name: ),   //e719
    //(Value: ;   Name: ),   //e71a
    //(Value: ;   Name: ),   //e71b
    (Value: ord(IconFilter);   Name: IconFilterPath),           //e71c
    //(Value: ;   Name: ),   //e71d
    (Value: ord(IconZoom);   Name: IconZoomPath),               //e71e
    (Value: ord(IconZoomOut);   Name: IconZoomOutPath),         //e71f
  //e720
    //(Value: ;   Name: ),   //e720
    (Value: ord(IconSearch);   Name: IconSearchPath),           //e721
    (Value: ord(IconCamera);   Name: IconCameraPath),           //e722
    (Value: ord(IconAttach);   Name: IconAttachPath),           //e723
    (Value: ord(IconSend);   Name: IconSendPath),               //e724
    (Value: ord(IconForward);   Name: IconForwardPath),         //e72a
    (Value: ord(IconBack);   Name: IconBackPath),               //e72b
    (Value: ord(IconRefresh);   Name: IconRefreshPath),         //e72c
    (Value: ord(IconShare);   Name: IconSharePath),             //e72d


  //e730
    (Value: ord(IconFavoriteStar); Name: IconFavoriteStarPath), //e734
    (Value: ord(IconFavoriteStarFill); Name: IconFavoriteStarFillPath),    //e735
    (Value: ord(IconCheckbox); Name: IconCheckboxPath),                    //e739
    (Value: ord(IconCheckboxComposite); Name: IconCheckboxCompositePath),  //e73a

  //e740
    (Value: ord(IconSwitchUser);   Name: IconSwitchUserPath),   //e748
    (Value: ord(IconPrint);   Name: IconPrintPath),             //e749
    (Value: ord(IconOEM);   Name: IconOEMPath),                 //e74c
    (Value: ord(IconDelete);   Name: IconDeletePath),           //e74d
    (Value: ord(IconSave);   Name: IconSavePath),               //e74e

  //e750
    (Value: ord(IconCloud);   Name: IconCloudPath),             //e753
  //e760
    (Value: ord(IconPageLeft);   Name: IconPageLeftPath),       //e760
    (Value: ord(IconPageRight);   Name: IconPageRightPath),     //e761
  //e770
    (Value: ord(IconGlobe);   Name: IconGlobePath),             //e774
    (Value: ord(IconContact);   Name: IconContactPath),         //e77b
  //e780
    (Value: ord(IconError);   Name: IconErrorPath),             //e783

  //e790
    (Value: ord(IconLight);   Name: IconLightPath),             //e793
  //e7a0
  //e7b0
    (Value: ord(IconRedEye);  Name: IconRedEyePath),            //e7b3
  //e7c0
  //e7d0
  //e7e0
    (Value: ord(IconPowerButton);  Name: IconPowerButtonPath)   //e7e8
  //e7f0
    );

  IconNamesE7: array of TIdentMapEntry = (
  //e700
    (Value: ord(IconGlobalNavButton); Name: IconGlobalNavButtonName),      //e700
    (Value: ord(IconWiFi);   Name: IconWiFiName),               //e701
    (Value: ord(IconBluetooth);   Name: IconBluetoothName),     //e702
    //(Value: ;   Name: ),   //e703
    //(Value: ;   Name: ),   //e704
    //(Value: ;   Name: ),   //e705
    (Value: ord(IconBrightness);   Name: IconBrightnessName),   //e706
    //(Value: ;   Name: ),   //e707
    //(Value: ;   Name: ),   //e708
    //(Value: ;   Name: ),   //e709
    //(Value: ;   Name: ),   //e70a
    //(Value: ;   Name: ),   //e70b
    //(Value: ;   Name: ),   //e70c
    (Value: ord(IconChevronDown); Name: IconChevronDownName),   //e70d
    (Value: ord(IconChevronUp);   Name: IconChevronUpName),     //e70e
    (Value: ord(IconEdit);   Name: IconEditName),               //e70f
  //e710
    (Value: ord(IconAdd);   Name: IconAddName),                 //e710
    (Value: ord(IconCancel);   Name: IconCancelName),           //e711
    (Value: ord(IconMore);   Name: IconMoreName),               //e712
    (Value: ord(IconSettings);   Name: IconSettingsName),       //e713
    (Value: ord(IconVideo);   Name: IconVideoName),             //e714
    (Value: ord(IconMail);   Name: IconMailName),               //e715
    (Value: ord(IconPeople);   Name: IconPeopleName),           //e716
    (Value: ord(IconPhone);   Name: IconPhoneName),             //e717
    (Value: ord(IconPin);   Name: IconPinName),                 //e718
    //(Value: ;   Name: ),   //e719
    //(Value: ;   Name: ),   //e71a
    //(Value: ;   Name: ),   //e71b
    (Value: ord(IconFilter);   Name: IconFilterName),           //e71c
    //(Value: ;   Name: ),   //e71d
    (Value: ord(IconZoom);   Name: IconZoomName),               //e71e
    (Value: ord(IconZoomOut);   Name: IconZoomOutName),         //e71f
  //e720
    //(Value: ;   Name: ),   //e720
    (Value: ord(IconSearch);   Name: IconSearchName),           //e721
    (Value: ord(IconCamera);   Name: IconCameraName),           //e722
    (Value: ord(IconAttach);   Name: IconAttachName),           //e723
    (Value: ord(IconSend);   Name: IconSendName),               //e724
    (Value: ord(IconForward);   Name: IconForwardName),         //e72a
    (Value: ord(IconBack);   Name: IconBackName),               //e72b
    (Value: ord(IconRefresh);   Name: IconRefreshName),         //e72c
    (Value: ord(IconShare);   Name: IconShareName),             //e72d


  //e730
    (Value: ord(IconFavoriteStar); Name: IconFavoriteStarName), //e734
    (Value: ord(IconFavoriteStarFill); Name: IconFavoriteStarFillName),    //e735
    (Value: ord(IconCheckbox); Name: IconCheckboxName),                    //e739
    (Value: ord(IconCheckboxComposite); Name: IconCheckboxCompositeName),  //e73a
  //e740
    (Value: ord(IconSwitchUser);   Name: IconSwitchUserName),   //e748
    (Value: ord(IconPrint);   Name: IconPrintName),             //e749
    (Value: ord(IconOEM);   Name: IconOEMName),                 //e74c
    (Value: ord(IconDelete);   Name: IconDeleteName),           //e74d
    (Value: ord(IconSave);   Name: IconSaveName),               //e74e
  //e750
    (Value: ord(IconCloud);   Name: IconCloudName),             //e753
  //e760
    (Value: ord(IconPageLeft);   Name: IconPageLeftName),       //e760
    (Value: ord(IconPageRight);   Name: IconPageRightName),     //e761
  //e770
    (Value: ord(IconGlobe);   Name: IconGlobeName),             //e774
    (Value: ord(IconContact);   Name: IconContactName),         //e77b
  //e780
    (Value: ord(IconError);   Name: IconErrorName),             //e783

  //e790
    (Value: ord(IconLight);   Name: IconLightName),             //e793
  //e7a0
  //e7b0
    (Value: ord(IconRedEye);  Name: IconRedEyeName),            //e7b3
  //e7c0
  //e7d0
  //e7e0
  (Value: ord(IconPowerButton);  Name: IconPowerButtonName)     //e7e8
  //e7f0
    );

  IconPathsE8: array of TIdentMapEntry = (
  //e800
    (Value: ord(IconHome);   Name: IconHomePath),               //e80f
  //e810
    (Value: ord(IconMapLayers);   Name: IconMapLayersPath),     //e81e
  //e830
    (Value: ord(IconFolderOpen);   Name: IconFolderOpenPath),   //e838
  //e880
    (Value: ord(IconUSB);   Name: IconUSBPath),                 //e88e
  //e890
    (Value: ord(IconView);   Name: IconViewPath),               //e890
    (Value: ord(IconSync);   Name: IconSyncPath),               //e895
    (Value: ord(IconDownload);   Name: IconDownloadPath),       //e896
    (Value: ord(IconHelp);   Name: IconHelpPath),               //e897
    (Value: ord(IconUpload);   Name: IconUploadPath),           //e898
    (Value: ord(IconMailForward);   Name: IconMailForwardPath), //e89c
  //e8a0
    (Value: ord(IconZoomIn);   Name: IconZoomInPath),           //e8a3
                                                                //e8a4
    (Value: ord(IconDocument);   Name: IconDocumentPath),       //e8a5
    (Value: ord(IconSwitch);   Name: IconSwitchPath),           //e8ab
  //e8b0
    (Value: ord(IconFolder);  Name: IconFolderPath),            //e8b7
    (Value: ord(IconPicture);  Name: IconPicturePath),          //e8b9
                                                                //e8ba
    (Value: ord(IconChromeClose);  Name: IconChromeClosePath),  //e8bb
                                                                //e8bc
    (Value: ord(IconMessage);  Name: IconMessagePath),          //e8bd
    (Value: ord(IconLeaf);  Name: IconLeafPath),                //e8be

  //e8c0
    (Value: ord(IconCharacters);   Name: IconCharactersPath),   //e8c1
    (Value: ord(IconMailReplyAll); Name: IconMailReplyAllPath), //e8c2
    (Value: ord(IconRead);  Name: IconMailReadPath),            //e8c3
    (Value: ord(IconCut);   Name: IconCutPath),                 //e8c6
    (Value: ord(IconMailReply);   Name: IconMailReplyPath),     //e8ca
    (Value: ord(IconSort);   Name: IconSortPath),               //e8cb
  //e8d0
    (Value: ord(IconFont);   Name: IconFontPath),               //e8d2
    (Value: ord(IconFontColor);   Name: IconFontColorPath),     //e8d3
  //e8e0
    (Value: ord(IconFontSize);   Name: IconFontSizePath),       //e8e9
    (Value: ord(IconTag);   Name: IconTagPath),                 //e8ec
    (Value: ord(IconCalculator);  Name: IconCalculatorPath),    //e8ef
  //e8f0
    (Value: ord(IconChatBubbles);   Name: IconChatBubblesPath), //e8f2
    (Value: ord(IconNewFolder);   Name: IconNewFolderPath),     //e8f4
    (Value: ord(IconAddFriend);   Name: IconAddFriendPath),     //e8fa
    (Value: ord(IconAccept);   Name: IconAcceptPath)            //e8fb
    );

  IconNamesE8: array of TIdentMapEntry = (
  //e800
    (Value: ord(IconHome);   Name: IconHomeName),               //e80f
  //e810
    (Value: ord(IconMapLayers);   Name: IconMapLayersName),     //e81e
  //e820
  //e830
    (Value: ord(IconFolderOpen);   Name: IconFolderOpenName),   //e838
  //e880
    (Value: ord(IconUSB);   Name: IconUSBName),                 //e88e
  //e890
    (Value: ord(IconView);   Name: IconViewName),               //e890
    (Value: ord(IconSync);   Name: IconSyncName),               //e895
    (Value: ord(IconDownload);   Name: IconDownloadName),       //e896
    (Value: ord(IconHelp);   Name: IconHelpName),               //e897
    (Value: ord(IconUpload);   Name: IconUploadName),           //e898
    (Value: ord(IconMailForward);   Name: IconMailForwardName), //e89c
  //e8a0
    (Value: ord(IconZoomIn);   Name: IconZoomInName),           //e8a3
    (Value: ord(IconDocument);   Name: IconDocumentName),       //e8a5
    (Value: ord(IconSwitch);   Name: IconSwitchName),           //e8ab
  //e8b0
    (Value: ord(IconFolder);  Name: IconFolderName),            //e8b7
    (Value: ord(IconPicture);  Name: IconPictureName),          //e8b9
                                                                //e8ba
    (Value: ord(IconChromeClose);  Name: IconChromeCloseName),  //e8bb
                                                                //e8bc
    (Value: ord(IconMessage);  Name: IconMessageName),          //e8bd
    (Value: ord(IconLeaf);  Name: IconLeafName),                //e8be

  //e8c0
    (Value: ord(IconCharacters);   Name: IconCharactersName),   //e8c1
    (Value: ord(IconMailReplyAll); Name: IconMailReplyAllName), //e8c2
    (Value: ord(IconRead);  Name: IconMailReadName),            //e8c3
    (Value: ord(IconCut);   Name: IconCutName),                 //e8c6
    (Value: ord(IconMailReply);   Name: IconMailReplyName),     //e8ca
    (Value: ord(IconSort);   Name: IconSortName),               //e8cb
  //e8d0
    (Value: ord(IconFont);   Name: IconFontName),               //e8d2
    (Value: ord(IconFontColor);   Name: IconFontColorName),     //e8d3
  //e8e0
    (Value: ord(IconFontSize);   Name: IconFontSizeName),       //e8e9
    (Value: ord(IconTag);   Name: IconTagName),                 //e8ec
    (Value: ord(IconCalculator);  Name: IconCalculatorName),    //e8ef
  //e8f0
    (Value: ord(IconChatBubbles);   Name: IconChatBubblesName), //e8f2
    (Value: ord(IconNewFolder);   Name: IconNewFolderName),     //e8f4
    (Value: ord(IconAddFriend);   Name: IconAddFriendName),     //e8fa
    (Value: ord(IconAccept);   Name: IconAcceptName)            //e8fb
    );

  IconPathsEC: array of TIdentMapEntry = (
    (Value: ord(IconDateTime);   Name: IconDateTimePath)            //ec92
  );
  IconNamesEC: array of TIdentMapEntry = (
    (Value: ord(IconDateTime);   Name: IconDateTimeName)            //ec92
  );

  function GetIconPathByIndex(Index: integer): string;
  function GetIconNameByIndex(Index: integer): string;
  function IdentToCodePoint(const Ident: string; out CodePoint: Longint): Boolean;
  function GetIconPathByIdent(const Ident: string): string;

implementation

function GetIconPathByIndex(Index: integer): string;
begin
  Result:= '';
  if Index<$e700 then exit;
  if Index<$e800 then
  begin
    IntToIdent(Index, Result, IconPathsE7);
    exit;
  end;
  if Index<$e900 then
  begin
    IntToIdent(Index, Result, IconPathsE8);
    exit;
  end;
  if Index<$ed00 then
  begin
    IntToIdent(Index, Result, IconPathsEC);
    exit;
  end;
end;

function GetIconNameByIndex(Index: integer): string;
begin
  Result:= '';
  if Index<$e700 then exit;
  if Index<$e800 then
  begin
    IntToIdent(Index, Result, IconNamesE7);
    exit;
  end;
  if Index<$e900 then
  begin
    IntToIdent(Index, Result, IconNamesE8);
    exit;
  end;
  if Index<$ed00 then
  begin
    IntToIdent(Index, Result, IconNamesEC);
    exit;
  end;
end;

function IdentToCodePoint(const Ident: string; out CodePoint: Longint
  ): Boolean;
begin
  if IdentToInt(Ident, CodePoint, IconNamesE7) then exit(true);
  if IdentToInt(Ident, CodePoint, IconNamesE8) then exit(true);
  if IdentToInt(Ident, CodePoint, IconNamesEC) then exit(true);
  CodePoint:= 0;
  Result:= false;
end;

function GetIconPathByIdent(const Ident: string): string;
var
  cp: integer;
begin
  Result:= '';
  if IdentToCodePoint(Ident, cp) then
  begin
    Result:= GetIconPathByIndex(cp);
  end;
end;

end.

