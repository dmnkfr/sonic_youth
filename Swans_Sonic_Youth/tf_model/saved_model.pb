??%
?-?-
D
AddV2
x"T
y"T
z"T"
Ttype:
2	??
B
AssignVariableOp
resource
value"dtype"
dtypetype?
~
BiasAdd

value"T	
bias"T
output"T" 
Ttype:
2	"-
data_formatstringNHWC:
NHWCNCHW
K
Bincount
arr
size
weights"T	
bins"T"
Ttype:
2	
Z
BroadcastTo

input"T
shape"Tidx
output"T"	
Ttype"
Tidxtype0:
2	
N
Cast	
x"SrcT	
y"DstT"
SrcTtype"
DstTtype"
Truncatebool( 
h
ConcatV2
values"T*N
axis"Tidx
output"T"
Nint(0"	
Ttype"
Tidxtype0:
2	
8
Const
output"dtype"
valuetensor"
dtypetype
?
Cumsum
x"T
axis"Tidx
out"T"
	exclusivebool( "
reversebool( " 
Ttype:
2	"
Tidxtype0:
2	
R
Equal
x"T
y"T
z
"	
Ttype"$
incompatible_shape_errorbool(?
W

ExpandDims

input"T
dim"Tdim
output"T"	
Ttype"
Tdimtype0:
2	
^
Fill
dims"
index_type

value"T
output"T"	
Ttype"

index_typetype0:
2	
?
GatherV2
params"Tparams
indices"Tindices
axis"Taxis
output"Tparams"

batch_dimsint "
Tparamstype"
Tindicestype:
2	"
Taxistype:
2	
=
Greater
x"T
y"T
z
"
Ttype:
2	
?
HashTableV2
table_handle"
	containerstring "
shared_namestring "!
use_node_name_sharingbool( "
	key_dtypetype"
value_dtypetype?
.
Identity

input"T
output"T"	
Ttype
:
Less
x"T
y"T
z
"
Ttype:
2	
l
LookupTableExportV2
table_handle
keys"Tkeys
values"Tvalues"
Tkeystype"
Tvaluestype?
w
LookupTableFindV2
table_handle
keys"Tin
default_value"Tout
values"Tout"
Tintype"
Touttype?
b
LookupTableImportV2
table_handle
keys"Tin
values"Tout"
Tintype"
Touttype?
q
MatMul
a"T
b"T
product"T"
transpose_abool( "
transpose_bbool( "
Ttype:

2	
?
Max

input"T
reduction_indices"Tidx
output"T"
	keep_dimsbool( " 
Ttype:
2	"
Tidxtype0:
2	
>
Maximum
x"T
y"T
z"T"
Ttype:
2	
e
MergeV2Checkpoints
checkpoint_prefixes
destination_prefix"
delete_old_dirsbool(?
>
Minimum
x"T
y"T
z"T"
Ttype:
2	
?
Mul
x"T
y"T
z"T"
Ttype:
2	?
?
MutableHashTableV2
table_handle"
	containerstring "
shared_namestring "!
use_node_name_sharingbool( "
	key_dtypetype"
value_dtypetype?

NoOp
M
Pack
values"T*N
output"T"
Nint(0"	
Ttype"
axisint 
?
PartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring 
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
?
Prod

input"T
reduction_indices"Tidx
output"T"
	keep_dimsbool( " 
Ttype:
2	"
Tidxtype0:
2	
e
Range
start"Tidx
limit"Tidx
delta"Tidx
output"Tidx"
Tidxtype0:
2		
@
ReadVariableOp
resource
value"dtype"
dtypetype?
@
RealDiv
x"T
y"T
z"T"
Ttype:
2	
E
Relu
features"T
activations"T"
Ttype:
2	
[
Reshape
tensor"T
shape"Tshape
output"T"	
Ttype"
Tshapetype0:
2	
?
ResourceGather
resource
indices"Tindices
output"dtype"

batch_dimsint "
validate_indicesbool("
dtypetype"
Tindicestype:
2	?
o
	RestoreV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
?
Select
	condition

t"T
e"T
output"T"	
Ttype
A
SelectV2
	condition

t"T
e"T
output"T"	
Ttype
P
Shape

input"T
output"out_type"	
Ttype"
out_typetype0:
2	
H
ShardedFilename
basename	
shard

num_shards
filename
0
Sigmoid
x"T
y"T"
Ttype:

2
N
Squeeze

input"T
output"T"	
Ttype"
squeeze_dims	list(int)
 (
?
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring ?
@
StaticRegexFullMatch	
input

output
"
patternstring
m
StaticRegexReplace	
input

output"
patternstring"
rewritestring"
replace_globalbool(
?
StridedSlice

input"T
begin"Index
end"Index
strides"Index
output"T"	
Ttype"
Indextype:
2	"

begin_maskint "
end_maskint "
ellipsis_maskint "
new_axis_maskint "
shrink_axis_maskint 
N

StringJoin
inputs*N

output"
Nint(0"
	separatorstring 
<
StringLower	
input

output"
encodingstring 
e
StringSplitV2	
input
sep
indices	

values	
shape	"
maxsplitint?????????
<
Sub
x"T
y"T
z"T"
Ttype:
2	
c
Tile

input"T
	multiples"
Tmultiples
output"T"	
Ttype"

Tmultiplestype0:
2	
?
UnsortedSegmentSum	
data"T
segment_ids"Tindices
num_segments"Tnumsegments
output"T" 
Ttype:
2	"
Tindicestype:
2	" 
Tnumsegmentstype0:
2	
?
VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"#
allowed_deviceslist(string)
 ?
E
Where

input"T	
index	"%
Ttype0
:
2	
"serve*2.6.02v2.6.0-rc2-32-g919f693420e8??$
?
embedding_15/embeddingsVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?N*(
shared_nameembedding_15/embeddings
?
+embedding_15/embeddings/Read/ReadVariableOpReadVariableOpembedding_15/embeddings*
_output_shapes
:	?N*
dtype0
z
dense_40/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:@* 
shared_namedense_40/kernel
s
#dense_40/kernel/Read/ReadVariableOpReadVariableOpdense_40/kernel*
_output_shapes

:@*
dtype0
r
dense_40/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:@*
shared_namedense_40/bias
k
!dense_40/bias/Read/ReadVariableOpReadVariableOpdense_40/bias*
_output_shapes
:@*
dtype0
z
dense_39/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:@* 
shared_namedense_39/kernel
s
#dense_39/kernel/Read/ReadVariableOpReadVariableOpdense_39/kernel*
_output_shapes

:@*
dtype0
r
dense_39/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense_39/bias
k
!dense_39/bias/Read/ReadVariableOpReadVariableOpdense_39/bias*
_output_shapes
:*
dtype0
?
StatefulPartitionedCallStatefulPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_18263
?
StatefulPartitionedCall_1StatefulPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_18268
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0
b
total_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	total_1
[
total_1/Read/ReadVariableOpReadVariableOptotal_1*
_output_shapes
: *
dtype0
b
count_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	count_1
[
count_1/Read/ReadVariableOpReadVariableOpcount_1*
_output_shapes
: *
dtype0
G
ConstConst*
_output_shapes
: *
dtype0	*
value	B	 R
H
Const_1Const*
_output_shapes
: *
dtype0*
valueB B 
I
Const_2Const*
_output_shapes
: *
dtype0	*
value	B	 R 
I
Const_3Const*
_output_shapes
: *
dtype0	*
value	B	 R 
??
Const_4Const*
_output_shapes	
:?"*
dtype0*ƚ
value??B???"BtheByouBiBandBaBinBtoByourBitBmyBofBmeBisBnaBweBonBitsBloveBnowBforBareBallBnoBlaBimBoutBdownBseeBthatBdontBwillBmindBnotBcomeBknowBwithBamBlightBwhatBwereBupByoureBbutBbeBlikeBthisBjustBheyBgoBillBtimeBcanBohBwhenBsoBfromBdoBnothingBfeelBneverBthereBherBsunBbodyBletBsheBbelieverBwasBeyesBtheresBrightBorBlifeBgotBwhereBhereBmanBgetBwayBintoByeahBwrongBtheyBgodBdeserveBaroundBoneBhaveBinsideBbackBthanBthroughBtakeBcantBawayBbelieveBusBrealBmoreBbetterBlittleBdayBatBfreeBchildrenBsoundBuhBneedBlieBhurtBgirlBownBhisBagainBourBheadBsomeBsayBmouthBhowBholdBthenBmakeBheBshesBbloodBmyselfBhandBshakeBsaveBpowerBhesBheartBfollowBcouldBgonnaBasBoverBifBwantBmotherBlookBfleshBbabyBsilverBmoneyBhateBbeenBwontBsoulBbyBwannaBeveryBthinkBalrightBwhoBwellBsongBheavenBanBtooBstarBmineBoffBletsBhimBgoingBdreamBtellBhardBnightBeyeBputBdeadBblackBthatsBsideBonlyBhandsBrememberBmoveBburningBahBeveryoneBearthBcomingBwhyBskyBmadeBkeepBforeverBtheirBhurtsBfaceBcloseBchildB	beautifulBaboutBuseBnewBheatBfriendBburyBturnBkissBhomeBbringBopenBhellBgoodBfightBdidBdiByesBgooBbeforeBthemBrunBrideBnameBhideBgoneBfallBcomesBshaBlunacyBfindBcityBthoughtBstayBshadowBnatureBlightsBhasBburnBbreatheBworldBthingBiveBheresB
everythingBdamnBcoldByoullBtodayBshouldBshoewaBsaidBgiveBbehindBstopBplaceBhadBgreatBanythingByourselfBwhiteBwhatsBstillBskinBsingBlongB	heartacheBcutBarmsBaliveBwaterBsweetBshowBlookingBhairBclearBboyBtrueBsugarBseaBsawBgroundBfearBeverBdogBdeepBcryBbigBbeneathBwhileBwalkBtouchB	somethingBsceneBhearBstrongBnailBmonaBhammerBforgetBfarBdreamsBdoorBcauseBairBwasteBtheseBsonicBsaysBredBrainBmuchBdidntBblueBblindBanotherBaaaahBwouldBtonightBtheyreBsunnyBsexBpeopleBlipsBbreathBalwaysBthingsBstandBmakingBleftBidBhmmBhitBgirlsBfeelsBcrazyB	worthlessBuntilBtownBshapeBsacredBriseBreallyBpleaseBmoneysBlineBlastBkillBfuckBfriendsBbedBbecauseByoungBwordBwaitingBpainBokayBnakedBmirrorBhopeBgreenBfireBbottomBbabeBwindBplasticBpartyBnobodyBlostBloseBlordBliveBhotBhiBgrowBfreedomBendBdearBblowBaintBacrossBwildBwalkingBvaB	somewhereBshotB	screamingBperfectBotherBmidnightBmenBmemoryB	lightningBleaveBknifeBinhumanBfeelingBdarkBcrossBcareBbornBanywayBandrogynousBalongByouveBwireBtruthBtrustBtoothBspiritBsecretBroomBriverBpoisonBpayBmeanBholeBhilatorBgoesBfloorBfitBeasyBeachBdreamingBdesireBdeathBbreakBanniBviolenceBveryBturnsBsonBsinBshineBrockBreachBplayBnicBmoonBliesBknowsB
incinerateBhuhBfirstBemptyBdirtyBcornerB	celebrityBcallingBbrokenBbecuzBbbbombsBangelBahoutBtroubleBtogetherBtalkingBspaceBsleeperBsickBsameBrunningBpastBoceanBnowhereBlonelyBgottaBgoldBgeBfuckingBeatBdoesBdieBcopBcontrolBcameBbuyBbossBanywhereBangelsBwishBtongueBthrowBtasteBstupidBstandingBsomeoneBsleepingBsandBroundBonceBmusclesBmeansBjosephBjesusBheardBgloryBgardenBfutureBflyingBflexB	everybodyBevenBendlessB	emptinessBcarBblackoutBbecomingBalcoholB2BunderBtwoBtreesBswayBswallowBstreetBstealBstartedBrollBrisingBoldBmovingBmothersBmeetBmakesBlayBhighBheartsBhealerBhangBfunBfoundBfallingBcallBbodiesBbestBaloneB	whistlingBwallBwaitB	universalBturningBtookBtoldBtightBthursdayBswearBstarsBroadBreadyBprincessBpraiseBmudBmissBmayBleadBinsaneBhangingBgoldenBgladBfuckedBenoughBcouldntBcleanBburnsBbreezeBbeginByearsBwoundBwonderBwithoutBvoiceB
understandBtearsB	sunfuckerB
summertimeBstareBspeakBsocietyBsevenBseenBseemsBsatisfyBradioBpushBpeaceBoohBmillionBmagicBloverBladderBkindBhopBhistoryBgrowingBglowingBforgiveBfloatingBflamesBerBdrinkBcriesBchestBcaughtBbrainsBboysBbearByoudBweveBusedBunfoldBundoneBtalkBsuckBstraightBstoreBsmileBsmellsBsmellBslowBsightBshitBsetB	sensationBscreenBsavedBrunsBquietBprotectBprayBpileBoutsideBmightBmeantBmaybeBmaryBlisaB	lifestyleBlaterBkneesBkissedBisntBhillBguessBgoodbyeBglassBfinallyBfieldBfailureBescapeBelseBedgeBdoneBdoingBcrackB	breathingBbrainBboneBbetweenBbadBamnesiaByetBworkBwhoreBwhichBwatchBuponBtryingBtreeBtrashBthroatBthoseBtearBsurpriseBstoneBspinsBspinningBsourBslippingBsleepBshellBsecondBprettyBpretendBpowersBpoorBplayingBontoBoilBnextBmindsBlooksBlandBlaidBknewBkingBkaneBimageBhouseBhoneyBholdsBheavensBgettingBflyBflowersBflashingBfallsBelectricBdrippingBdoesntBdaysBcrawlBcoolBcompleteBcolorsBcloserBcatchBbuttonBburiedBblissBbleedBblameBbitBbeyondBbecomeBbeatsBarrowBanyByoursBworshipBwordsBwinBwastedBwashingBwarmBwallsBuhhuhBturnedBtryBtopBtimesBtillBthoughBtangledB	surrenderBsuckingBsorryBslipBsingingBsimpleBsilentBshutBservantBsakeBreelingBreasonBpoursBpleasureBpinkBneckBnearBmustBmorningBlivingBlegsBlateBkoolBkneelBkarenBjoniBjobBholyBhelpBheadsBguitarBgodsBghostsBfunnyBfullBfillBexistBendsBdryBdrunkBdrivingBdrinkingBcrownBcandleBbrightB	breakdownBbesideBbelowBbellyBbeingBaskingBahahoutBzombieBwhoaBwashBvalleyBuniverseB	underwearB	toussaintBtiredBthruBtakesBsurviveBsuchBstrikesBstepBsteelBspitBsparksBskullB	sacrificeBreleaseBregretBrealityBreadBratsBpullBnoiseBnationBmusicBmmmBmakerBlotBlookedBliarsBliarBlearnBkillerBkidsBhollowBgunBgaveBflowerBfineBfeedBfatherBfactBdustBdriftBdiseaseBdevilBcrystalBcryingBcrawlingBcountBchanceBchainBcancerBcalledBbreathesBbooksBblockBbleedsBbiggerB	authorityBattackB3ByumyabByrByellowBworksBwindowBwentBweightBwearBwatchingBwasntBunwindBtwistedBtornBthinksBthinkingBthankBsundayBstrikeBstoryBstickBstartBstairsBstageBspreadBspendB	soundlessB	sometimesBsomebodyBsolidBsoftBsmokeBslowlyBslaveBsinkBsignBshowsBshadowsBscrewingBscoreBscaredBroseBrippedBridingBrestB
resistanceBreachingBrageBquiteBpuBprisonBpierceBpageBnightsB	neighborsBmurderBmomBmilesBmildredBmetBmachineBluckBleavesBlearnedBlalalalalalalalalaaaaaahBkeyBjimB	innocenceBiceBhighwayBhelplessB	heartbeatBhealBgraveBfrozenBforceBflamingBfistBfingersBfeltBfasterB
everywhereBdyingBdollarBdizzyBdigBdaughterBdarknessBcottonB	confusionBchooseB	childhoodBchangeBcatholicBbrushBbrotherBbrandBboundBbonesBbluesB	bloodlessBbeautyBbeatBbeastB	automaticBashamedBarentBamericaBagainstBaboveByorkByaBwithinBwhosBwearingBweakBwantedBvrBveinsBunbornBtvBtotalBtiedBthrillBthreeBthousandBthoughtsBsureBstringBstrangerBstrangeBstationBstateBsnowBsmallBslidingBskiesBsittingBsitBsignsBshinesBsendBsecretsBscratchBscrapeBsayingBsatisfactionBsangreBsaltyBsaintBsadBrowBrocketBriskBripBriotBringBrhythmBretinalBresistBrenegadeBreminderBrealizeBqueenBpurringBpurposeBpullingBpoolBpictureB
perfectionBpassBpaintedBpaintBorderBobsceneBoBnewsBmessBmemorysBmadelineBlungB
lonelinessBlitBliquidBleatherBkillingBjusticeBjumpBi’mBitllBinnocentBideasBhundredBhowlBhappenBgrowsBgreasyBgrassBgoddessBglitterBgatherBgainBfurtherBfreezeBfourB	forgottenBflameBfishBfiresBfellBfeetBfastBfakeB
experienceBexceptBeveningBeternityBemBedenBeatsBeaterBearlyBdrugBdriveBdressBdrainBdoubtBdogsBdisappearingB	differentBdestructionBdawnBcrimeBcriedBcreamBcontainBcoatBclubBcloudsBchangingBburninBbuildB	bubblegumBbringsBbreastsBbreastBbraveBboxBbootsBbookBbellsBaskBannalineBagoBafraidBacidB69BzeroB	yesterdayByawnBwomenBwomanBwishingBwinterBwinkingBwideBwholeBwheelsBwalkinBvrrBvoidBviolentBviewBunusualBundertowBundergroundBuglyBtriesBtouchedBtomorrowBtokyoBthrewBtenderBtenBtellsBteenageBteachBtaughtBtakingBswimmingB	swallowedBsuperBsufferBsucksBsuckerBstuckBstripBstreetsBstonesBstepsBstealingBstaresBstainedBsstBspecialBsoonBsodapopBsnakeBsmearBslipsBskirtBsisterBsingsBsinceBsilenceBshootBsheetB	shatteredBselfB	searchingBscreamBschoolBrustBruleB	righteousBrichBrevengeB	renegadesB	recognizeBratherBradiosBpunishBpromiseBpocketBpassingBpanicBothersBonesBnurseBnumberBnervesBneitherBnaturalBmysteryBmovieBmovesBmostBmoodBmomentBmistakeBmiseryBmirrorsBminutesBmindlessBmercyBmeadowBlowBloversB
louvertureBlivesBlisasBlipstickBlilifeBlifesBleastBleakingBlaughingBlaughBlaneBkindaBkeeperBjoyBjaneBjailBinstanceBhungryBhoseBhorseBhiddenBhelloBheldBheavyBheadacheBguyBglowBgloriousBgimmeBfuzzyBfrontBfreakBforgotBfoolBfoldBflowBfliesB
fingertipsBfilledBfieldsBfemaleBfeelingsBfeedingBfebBfamousBfadedBfacesBevilBeverydayB
everybodysBesBenjoyBememememememememB	emanatingBdropBdriftingBdressedBdreamedBdrainedBdisguiseBdisconnectedBdirtBdiggingBdiesBdespiseBdeBdaydreamBdateBcurlB	cultivateBcruelBcrimsonBcreepingBcreationBcreateBconsumeBconcreteBcolumnsBcolumnBcolorBcloudBchristBcharlieBcaveBcannotBcandyBburnedBboutBblowingBbloodyBblindedBbetrayBbegBawakeBaveBassBasleepBapartBampBaiaiaiaiaiaiaiaiBafterB03BwriteBwristBwreckBwrappedBworkedBwiresBwingsBwindowsBwhoseBwhisperBwheelBwestBweatherBwearyBwaveBwartimeBwarBveinBveilBvacationBuntrueB	unfoldingB	unchainedBumBtwistabilityB	turquoiseBtunnelBtruckBtripBtriedBtransmittingB
translatorBtrailBtoughBtossBtinyBtinBticketBthurstonBthinBthickBterrorBterribleB
temptationBteethBtallBswirlBsupposedB
superstoreBsunshineBsunsetBsunlightBsummerB	sufferingBsubwayBstyleBstupiderBstretchBstrengthBstreamBstinkBsticksBsteamBstationsBstatesBstainBsqueezeB
springtimeBspokenBsplitBspillingBspellBspeedBsootBsonsBsnapBsmartBsmallerBslideBsleeveBslapBslabBsitsBsirensBsinkingBsignificantB	shrinkingBshoulderBshoreBshiverBshipBshiningBshareBshallBshakingBsexyB	senselessBselfishBseemBseekingBseekBseedsBseedBscreamsBsandsBsailorsBsafeBruinsBrollsBrollingBridBrichardBrhymeB
reflectionB	reflectedBrecordBratBraptureBradicalB	quicksandB	questionsBquarterBpurpleBpureBpriestBpriceBpressedBprayerBpointBpoetBplanBpitBpigBphoneBpetBpatternBpatBparadiseBpaperBpantsBpaintersB	ourselvesBordinaryBorangeBobjectBnotionsBnothingsBnoneBneutralBmysticBmustveBmovementB	mountainsBmonsterBmonkeyBmommaBmirageBmilleBmilkBmeetsB
meditationBmatchBmassageBmaskBmarsBlyingBluckyBlooseBlongerBlocusBloadBlistenBlinedB	lightlessBliedBlickingB
liberationB
lemonheadsBleavingBleafBlambBlakeBkneelingBkittyBkittenBkissabilityBkidBkickBkeepsBkansaiBjupiterBjohnnyBjetBjellyBjaggedBjackBinvestBinsectsBinjectedBinfiniteBideaBhurryBhowlingBhoursB	horsehairBhollerBholesBholdingBhoBhitsBhipsBhipBhigherBhidingBheatherBhazyBhaterBgrewBgraceBghostBgateBgasolineBgasBgameBfurBfrigidBfreezingBfreedomsBformlessBformingBformBfogBfloodB
flashlightBfixBfishyBfingerBfilthyBfillsBfifteenBfewBfeuilleBfeelerBfearlessBfavoriteBfairBfailBfadingB
explosionsBegoBeggsBdullBdudeBdrunkenBdreamtB
dissolvingB
displacingB
discoveredB
disappearsB	disappearBdiosBdiedBdiamondBdewBdestroysBdestroyBdesertBdescribeBdenyBdeformBdeerBdecayBdaylightBdareBdancingBdanceBdampBcupBcuntBcrushBcrueltyBcrowdB	crimebossBcreatureBcrackedBcoupleBcorruptB	containedBcompanyBcoffeeBcockerBclosedBclockBclawsBclawBcitysBchromeBchokingBcheapBchasteBchasedBchainsBcausingB	castraterBcarsBcampBbuckBbothBbombBbodysBboatBblinkBblessBbleedingBblastBbirthBbindsB	believingBbegunBbeginsBbandB	backwardsBbaaBartistsBarmBantihateBantennaBancientBamorBalmostBalibiBalarmedBahhBabuseB10ByouthByoungerByehByeaBxrayBwrittenBwrenchBwrapsBwouldntBworthBworryBworkingBwoodsBwoodB	wonderingBwombBwokeBwishesBwisdomBwildflowersBwhoveB
whisperingBwhippingBwhetherBwesternBweepingBweepBweekendBwedBwavesBwattBwatchedBwantsBwanderBwalksBwalkedBvineBvidaBvictimBvacuumBuselessB	upsettingBuproarB	unwillingB	unrealityB	unnaturalBunkindBunityBungluedBunfortunatelyBunfortunateBunderstandingB	uncoveredBuncontainedBunconsciousBuhohBtypeaguyBtwistingBturboB	tricksterBtrebleBtransmitterBtrainBtragicBtraceBtotallyBtopsBtoolBtonguesBtonesoulBtombBtollBtigersBtickBthroneBthat’sBthatveBtestBtechBteacherBtackedBsymbolsBswordBswingBsweepBsweatBswampB	surroundsBsurfaceBsupermenB
sunshiningBsundryBsummersBsuicideBsuggestBsuffersBsuddenlyBstuffB	strongestBstrongerBstrippedB	stretchedBstreamxsonikBstrayBstratosphereBstrapB	strangledBstormyBstormBstoriesBstoppedBstomachsBstomachBstoleBstirBstimulationBstereoBsteppingBstaticBstairBstainsBsquishBsquareBspysBsprayBspotsBspliceBspitsBspiteB	spiralingBspiralBspineBspentBspeechBsparkleBsownBsoundsBsoundingBsoulsBsorrowBsoreBsoldierBsoldBsoilBsoakingBsnareBsmilingBsmashedBslippedBslightlyBsleazyBslashBslamsBskimmingBskatingBsizeBsixBsisBsirBsingleBsingerBsimplyBsightsB	sightlessBsighingBsighBsidewaysBshuttingBshownBshotgunBshortBshirtBshipsBshiftsBsheetsBsheenBshardsBshapingBshameBsettingBserveBsentBsenseBsemenBselflessB
selfesteemBselfdeceptionB	selectionBseasonsBseasBsearchBsealBscumB
scripturesBscrewB
screechingBscreamedBscrapingBscootBschizoactionBschemerB	scatteredBscatterBscapeBscansB	salvationBsaltBsailBsadieBrustyBrushingBrushBruinedBruinBrudeBroughBrotBrosesBropeBrootBrockingBrockedBroarsBriversBrewardBretinaBresultBrefrainBredneckBrecordsBreceiveBrapeBranchBraiseBradiumBquiverBquickBquestionBpushingBpusBpurifiedBpunkerB
punishmentBpunishedBpumpsBpulseBpublicBproudBproblemBprizeB	princesssB	priestoidBpressB
presidentsBprepareBpreciousBpowderedB	possessedBposeBpopB	pollutingBpollutedBpolishedB	pleasuresBplaysBplayedBplanetBpinkingBpigsBpieceBpieBpickBphysicalB
permissionB
perceptionBpeekabooBpavementBparkBparadeBpantyBpantherBpalmsBpaleBpagesBpaddleBoriginsB
oppressionBoceansB
obsessionsBobsessedBnumbersBnoticeBnorthBnononononothingBnomadsBnineBniceBnetBneonBneedlesBnecessarilyBneatBnaziBnamesBnBmurderedBmuddyBmovedBmotionBmopBmooreB	moonlightBmommasBmodernBmixBmissionBminusBmiddayBmetersBmergeBmentionBmentalBmeltedB
mechanicalBmeaningBmatterBmasterBmarkBmarilynBmapBmanyBmalibuBmailBmachinesBlustBlushBlungsBlovingBlovelyBlovedBlosingBlookinBlocksBlockingBlinesBlimitBlimeBlimbsBlikesBlightedBlifeboatBlickBlevysB
levitatingBlessonBleperBlegendBlegallyBlegacyBleaverBleakyBlaysBladyBlackBlaceBlabelB	knowledgeBknotBknockBkneeledBkneeBkiteBkissesBkinBkillersBkeptBkeepingBkarensBjunkBjuiceBjudgeBjoinBjeansBjanisBjamsBjacknifeBi…B	invisibleBinvisibilityBinterventionBinnerBinkBinjuredBinhaleBinclinedBincenseB	immovableBimaginationBignorantBidentityBidealsBhüskerBhyperstationBhunterBhungBhumiliationsBhumansBhumanBhugBhorsesBhmahmahmahmaBhedBheaterBhavingBhatsBhatredBhatefulBhardestBhappyB	happinessBhallBhalfBhaBhBguysBgutsBgutBguiltBguideBgrowthBgrooveBgrindingBgownBgodforsakenBgnarlB
glossariesB
glisteningBglaucomaBgivingBgivenBgirlfriendsBgiftBgetsBgestureBgeniusBgazeBgayBgaspBfuturesB
frustratedBfreshBfrenchBfreelyBframeBfragmentationBfountainBforthBformsBfoolishBfoodBfocusBflaringBfirefightersBfeverBfeedsBfathersBfateBfatalBfareB	fantasticBfamilyBfameBfalseBfallowBfaithBfadeB	explosionBexpectBexhaleB
exercisingB	everytimeBeverlastingB
eventuallyBerectionBequationBentertainerBentersBenginesBengineB	endlesslyBendingBendeavorBeasternBeasilyBeaseBearBdüBdullsBdueBdrumBdrugsBdrownedBdrownBdropsBdreamerBdoubleBdopeBdoorsBdoomedBdon’tBdonutsBdollBdodgingB	distortedBdistanceBdissolveBdisownedB
disgustingBdisconnectionB	disciplesB
directionsB	directionB
dimensionsB	dictationBdiamondsBdetailsBdespairB	demandingBdeliverBdelicateBdegradeB	deceptionBdeceiveBdebrisBdealBdeadeyedBdaydreamingBdarlingsB	dangerousBdangerBdandyBdancerB	damnationBdailyBcuttingBcutsBcurtainBcureBcrushedB	crumblingBcrowdedBcrouchBcriticalBcrashingBcrankingBcowboyBcowBcoverBcourseB	countdownBcorrodedBcordB
copkillingBcontactBconsumedB
constraintBconfessB	concealedBcommonBcomfortBcombustibleBcoloursBcoloredBcollieBcoalBcloverBclothesBclosingBclosetBclosesBclimbB	clevelandBclearlyBcleaningBclayBclaimsBcircleB
cinderellaB	cigaretteBciaB	childrensBcheeksBcheckBcharmBchaosBcertainBcenterBcellBcelebratingBceaseBcashBcarpetBcanyonsBbyeBbuysBbusyBburstBbunnyBbunchBbumpedBbullshitBbullB	buildingsBbrownBbrideBbridalBbreaksBbowingBbowBboughtBboostBblushBblowsBbloozeBblastedBbitchBbirthdayBbirdBbindBbettingBbendBbehaveBbeesBbecomesB
bbbblessedBbattleBbangingBbakeBbagBbaconBawBavenueBaudienceB	attentionB
atmosphereBasideBarrivedB
apparentlyBanyoneBanyhowB
antiorgasmBanothersBanimateBanimalBambitionBalsoBalreadyBallowBaliceBaheadBagonyBagesBafricaBadvertisementsBadoreB	addictionBabstractBabruptlyBableBaahBaaaaaaaaaaahB40B30B2015B20BzoomBzoftigBzippersBzipperBzestBzeroesByowlByonderByogiByknowB
yesterdaysByellByearByardsByardBxgirlBwroteBwrong…BwritingBwrithingBwritheBwritesBwrappingBwowBwoundsBwoundedBwouldveBwormsBworeB	wonderfulBwonderedBwomensBwoahBwizardBwitnessBwitchBwiselyBwiseB
wintertimeBwinkBwineBwindsBwindingBwillingB
wildflowerBwigBwifesBwifeBwieldBwhoooowB	whoooooowB	wholesaleBwhodBwhiteskinnedBwhitehotBwhistlesBwhisperyB	whisperedBwhiskedBwhirringB	whirlpoolBwhirlingBwhipsBwhippedB	whipcreamBwhipBwhimBwheredBwheneverBwhateverBwhatdBwhammyBwe’reBwetBwernerBwerewolfBweirdBweighsBweeksBweedsB	wednesdayBweddingB
weathermanBwealthyBweaknessBweakerBwaysBwaxBwavoBwavingBwatcherB	wastewoodBwashoutsBwashedB	washclothBwarriorBwantingB	wanderingBwallopBwakingBwaitinBwaitiBwahooBwahBwadBvisionBviolatorB	violationBviciousBviceB	vicariousB	vibrationBvibrateBvestBvesselBventBvendellaBvelvetBvapidBvanBvainBvacantButopiaBuptightBunusedBunspokenBunseenBunrealBunprotectedBunmadeBunlockBunloadBunlikeBunitedB	unfurlingBunforgivingBundoingBundoB
undetectedB
underworldB
understoodB
underneathBunderfedB
underbellyBunconsciousnessBunclearB	unchangedBunburiedB	unbuckledBultimateBtyingBtwo brokenBtwisterBtwistBtwirlBtwinkleBtwineBtwinBtwiceBtwerpBtwentyBtvsetBtunnelsBtunicsBtunicBtunesBtuneBtuffBtuckBtshirtBtruthsBtrustedBtrunkB
truncheonsBtrulyBtrudiBtrucksBtroubledBtrixBtripsBtrippingBtriggerBtricksBtrickBtreasonBtreadBtrapsBtrapB
transpiredBtransparentBtransmissionB
transformsBtransformerB	transformBtranscendenceBtrancesB	traipsingB	trainyardBtrainsBtracksBtrackBtoysBtowelBtowardsBtowBtourBtouchingB	touchdownBtotaledBtossingBtorturedBtoreBtorchBtonightsBtonBtomsB	tomorrowsB	tombstoneBtomatoBtomBtoilingBtoesBtoasterBtoastBtitleBtitBtiresBtinglingBtimereversedBtiltedBtightestB
tighteningBtiesBtieBtidesBtideBtickleBticketpatchBthyB
thunderingBthrownBthrowingBthrongB	throbbingBthrobBthroatsBthriveBthrillsB	threadingBthrashBthornsBthirtyBthirteenBthirstBthirdBthin white skinBthighsBthey’dBtheresasBtheresaBtheoryBtheaterBthatllBtextsBtexasBtetheredBtestingB
terrorizedBtermBtentBtenseBtemptingBtemptBtemporarilyBtemperB	telepathyBteenBteeBteaserBteachersBteaBtaxiBtattooedBtattooBtastingBtastesBtarBtantricBtanBtampBtalksBtalkinBtaleBtailsBtailBtadumdumBtableBsyntaxBsymphonyBswornBswoopBswitchBswirlingBswimBswillB
sweetshineB
sweetheartBsweaterBswansBswallowsBswallB	sustainedBsurelyBsupremeBsupposeBsupineB	supersoulBsuperiorB	superfineB
superchunkB	supercashBsuperboyBsupBsunsB	sunkissedBsulphurBsullenB	suitcasesBsugarcoatedBsuffocatingBsuddenBsuckeredBsuckedBsuccubiBsucceedBsuburbanBsubtleB	substanceB
subscribesB
submissionBsublimeB	stumblingBstubbornBstruggleB	structureBstruckBstronglyBstrollBstrodeBstrideB
streetwiseBstreetmatikB	streamingBstreamedBstreaksBstrayingB
strawberryBstrataBstrangleB	strangersBstoveBstoresBstoodBstolenBstinksBstinkingBstinkinBstiltedBstickyBsteveBstereographicBsteamingBstealsBstaysBstayingBstayedBstarvingBstarvedBstaringB	starfieldBstardustBstandsBstampsBstacksBstaceyBstabBstBsssssshB	squishingBsquirtB	squattingBsquadsB	spreadingBsprayingBspotBspoonBspokesBsplitterBsplitsBspleenBsplayedBsplashBspiritsB
spirallingBspinninBspinnedBspinBspillsBspilledBspillBspiderBsphereBspectacularBspearBspeaksBspeakersBspasticB	sparklingBsparklesBspareBspanishBspainBsouthBsoundscapesBsoufflesBsortBsorelyBsoonerBsonarsBsometimeB
somethingsB	someplaceBsomeonesBsomedayB
solidifiedBsolarBsoapBsoakBsnowgirlBsnoopeeBsneakingBsneakersBsneakedB	snatchingBsmokesBsmilesB	smilelessBsmiledBsmellingBsmashingBsmashBslunkB
slipstreamBslimeBslidesBslickBsliceBsleepsoBsleepsBsledB	slaveshipBslavesBslashersBskyscrapersBskylightBskullsBskirtsBskinnyBskinheadBskilletBskidsBsixtyBsixteenB	sixsexsixBsiteBsistersBsirenBsinnersBsinnerBsimultaneousBsillyBsignemBsignalsBsignalBsiftingBsiftBsiegBsidewalkBsidelongBsickestBshyB	shriveledBshowingBshowerBshowdownBshoveBshoutingBshoutBshouldntBshotsBshoppingBshopBshootsBshootingBshoneBshoesBshockingBshockBshinyBshimmerBshiftingBshiesBshieldedBshermanB
shelteringBshelterBshaveB	shapelessBshampooB	shamelessBshakyBshadyBsexualBsexlessBsexismeBsewersBsetsBservesBseriousBseparateBsentientBsensoidBsensesB
sensationsBsensateBsenileBsendsBsendingBsellBselfreflectingBselfdeludedBseizeBseesBseepingB	seeminglyBseemedBseekerBseeingBseduceBseasonBseasideBsealedBseafoodBscrewedBscoutBscornedBscornBscorchBscoldB
scientistsB
schoolyardBschmoozeBschizophreniaBscattershotBscarsBscarletBscannedB
saxophonesBsavingBsavesBsavageB
saturnaliaB	satisfiedB
satellitesB	satelliteBsatansBsatanBsaneBsalutationsBsaleBsaladB	salaciousBsaintsBsaintlyBsailingB	sailboatsBsailboatBsagsBsadnessBsadlyB
sacrificedBrustingBrunawayBrumblingBruffleBrubbleBrubberBrubBrouteBroustedBrottingBrottenBrotationBroshumbaBrootsB	roommatesBroofBromanceBrolledBrocksBrockinBrockersBrobotBrobeBriveredBriteBriskingBrisesBrippingB
rightfullyBrifleBriffsBriffBridgeBrhymesBrhodeBrevoltBreversedBreverseB	revealingBrevealBreturnedBreturnBreservedBrequiredB
reputationB
repressionBreplayB	replacingBreplacedBreplaceBrenounceBrenewBremovingBremorseBreminiscentB	remindingBremakeBremainsBremainBreliveBreligionB	releasingBrelativeBrelationshipBrehearseBrehabilitationBregretsB	reformingB
reflectingBreentryBreedBreductionistBredderB	recordingBreconnoiteringB	recommendBrecognitionsBrecognitionBreckonBreceiverBrecedingBrebarBreadingB	reactionsBreachesBraysB
ravagementBraunchBrattlingBrapesB	rapaciousBransomBrangBrandomBranBraisingBraisedBrainbowsBrainbowBrailroadBrailBragingBragedBradarBrabiesBrabbitB	quiveringBquartersBquaintBpyreBputsBpussysBpussBpushedBpursueBpupsBpuppetsBpupBpunkB	punishingBpullsBpulledBpukesBpubBpsychoB
providenceBprovedBproveBprovableBprotestBpromisedBproductBprocreationBprivacyBprintoutBprinceBpricklyBprickedB	preverbalBpreternaturallyBpressingB	presidentBpreserveB	presentedBpremiseBpreconceivedB	precipiceBpreacherBprayersBpracticallyBpowderBpowBpouredBpourBpoundingBpoundBpounceBpotsBpotatoBpostersBpostBpossibilitysBpossibilityBposerBpoolsBpoodleB
ponytailedBpontiacB	polishingBpoliceBpokedBpokeBpoisedBpointsBpointingBpoetsBpoeticsBpoeticBpoemsBpocketsBplungerBplumesBplumeBplumBplugBpleezeBpleatedBpleasuristicBpleasedBpleasantBpleasBplayersBplatformBplateauBplasterBplacesBpityB	pitchforkBpissedBpissBpipelineBpinBpillarsBpillBpiledBpierBpiedmontBpiecesBpicturesB	phospheneB
phonographB	phonecallBphaseB	petroleumBperversionsBpersonBperiodB
perceptiveBperceiveBpepperBpenetrationBpenetratingBpeepingBpedalBpeachBpaysBpaveBpaulinaBpatientBpatentBpatchesBpatchBpasteBpassiveBpassionsB	particlesBpartBparallelBpansBpanesBpalmBpalloredBpairB	paintingsBpaintingBpainsBpainfulBpaidBpadsBpackedBpackBoxygenBoverwhelmedBovertimeBovertakeB	overnightBoutrageouslyBoutlooksBoutdateBoursBounceBoughtaBouchBosakaB	organizerBorganBordureBordersBorchidsBorchardBorangesBoraclesBopticBoppositeBopiumB	operationBopensBopeningBopenedBoopsBoooooooooohB
ooooooooohBooohBoooBoleBoffalBoctaveBochreBobscenitiesBoblivionB	obedienceBoaksBnymphoidBnylonBnutsBnurtureBnumberedBnullifyBnullBnourishBnothingnessBnoteBnorthernBnooneBnoonB	noiseballBnoddingBnipplesB
ninetynineB
newspapersBnewbornBnever caughtB
never beBnestBneilBneighingB
neighboursBneedsBneedingBnearlyBnaturefriendBnativeBnarcsB
narcotizedBnarcoticBnaomiBnamedBnailsBnadaBmythB
mutilationBmutateB	musicallyBmuscleB	murderingBmurdererBmumBmovinBmouthsBmountedBmountainBmotorBmotionedBmotherfatherBmoteBmotaBmoralBmooningBmontereyBmonroeBmomentsB	moleculesBmoistBmodelBmobBmoansBmixupBmixingBmisuseBmisunderstoodBmistingBmisteryBmistakenBmissingBmissilesBmissileB	misshapenBmisshapeBmissedBmisledBmirroredBminuteBmindlesslessB
millenniumBmildBmightyBmiddleB
methedrineBmethaneBmetermanBmetallicBmessyBmessingBmerestBmendBmenaceBmemorizeBmeltsBmeltingBmeltBmelodiesB	meatbloodBmeatBmeanoBmeaninglessBmealsBmazzyBmazeB	maxfieldsBmattressBmasturbatingBmasturbatesB
masondixonBmarshallBmarrowBmarriesBmarredBmarlboroBmarkerB	marijuanaBmariahBmarchBmappingBmantoolBmansionBmansBmanneredBmannerBmanifestationsBmanifestBmaniacBmammalBmallsB	malleableB	malignantBmaleBmaintainB	magnesiumBmagickBmagazineBmadnessBluxuryBlunchBlullabyBlowbeamBlovesBlouBlotsBlossBloserBlonesomeBlonerBloftBlockBlocationBloatheBlizardBlivinBlivedB	literallyB	listeningBlistBlippedBlipBlionBliningBlimitationsBlikedBlightmapBlightingBliftingBlieveB	librariesBlibertéBliberateBlevyBlevitateBletterBlethalBlessBleonineBlensBlennyBlendBleisureBledBlearningBleaptBleapingBleaningBleachingB
lazymindedBlazyBlaundryBlaughterBlaughedBlatherBlatheBlashBlarryBlarkBlapsedBlapBlanguishingBlanguidBlandingBlambsB	lafayetteBladysBlabyrinthineB
kukluxklanBkrillBknownBknowingBknobBknivesB	knifeholeBkneltBkixBkissingBkingdomBkindnessBkillsBkilledBkickingBkickedBkeysBkeepersBjuvenileBjunkiesBjungleBjukeboxBjugBjudithBjr​BjoyousBjournalBjoplinBjonisBjohnsBjohnBjogsBjoeBjobsBjoanBjivingBjesseBjessBjerksBjerkB
jeanmichelBjawsBjasmineBjarsBjamBjacketBi’llBit’sB
its lostBitselfBitchB	isolationBislandboundBislandBirreversibleBirrealBironBirisBin aBinvertedB	intuitionB	introduceB
intestinesB
interwovenBintertwinedBinternationalBinterlockedBintelligenceB
insulationBinstrumentalBinsertBinsectB
inkstainedB	injectingBinjectBinitialBinhalingB
infuriatedBinformationB	influenceB	infinitysBinfinityB
infectionsB	infectionB	ineffableBindustryBindividuallyBindifferenceBindianBindexBindecipherableB	increasedBincisionBinchingBimpureB	impreciseBimpotentB	impotenceB
impossibleB	importantBimpersonatingB
immoveableBimmersedB	immediateBimitateBimbecileBimaginesBimaginationsBimagesB
illuminateBiimB iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiBignoreB	ignoranceBignitionBignitedBidolsBidleBidiotsBiaohaiaaohaiauhaiaiiuhaB
hypostaticB	hypocrisyB
hypnotizesB
hypertonicBhypernationBhypedoutBhylozoicBhutBhuntBhungerBhundredsBhunchBhummingBhumiliationBhuhhuhhuhyeahBhuggedBhubcapsBhowsBhoverBhospitalBhorrorBhornsBhorizonBhopingB
hopelesslyBhonestyB	hollywoodBhogsBhiveBhitchBhipstersBhippiesBhijoBhighestBhifiBhieroglyphicloverBhidesB
hesitationBherselfBheroesBheroBhelpingBhelmetsBheilinBheavingBheatseekingBheartbreakerBhealingBhealedBheadsplitterBheadlineBhazeBhauntedBhatBharvestBhardlyB
hardearnedBhappilyB	happeningBhandcutBhandcuffB	hampshireBhamBhaloBhallucinatedBhallsBhallowBhalfmanB	halflightBhaikuBhahaBhadntBgunsBgumBgulleyBguitarsBguiltyBguidingBguestBguardingBguardianBguardBgrownBgroupBgroovyBgroovedBgroceryBgroansBgroanBgritBgrinBgrimyBgrillBgreyBgrenadeBgreeneBgreedBgreasingBgreasedBgreaseBgrazingBgrayBgratefulBgraniteBgrammysBgradeBgracefulBgrabbingBgoldmineBgogetterBgodheadB	goddamnedBgoddamnBgoalsBgoalBglueBglovesBglitzB
glitteringBglistensBglancingBglanceBglamourBgladlyBgivesBgigBghostingBghettoBgetterBgentlyBgentleBgeeseBgearsBgauzeBgatesBgashBgardensBgarbageBgamesB	gallopingBgalleryBgainedBgailBgagBgBfutilityBfunkBfuneralBfundamentalistBfulfillmentBfuelingBfueledBfuelBfucksBfrustrationBfrozeBfrogsBfrogBfrightBfriedBfretBfrenziedBfreewaysB	freespeedBfreckledBfreakyBfreaksBfreakedBfrazzledBfraternitéB	fragmentsB	fracturedBfoxBfourteenB	fouralarmBfoundingBfoulBfoughtBforwardB	fortunateBforsakeBforkB	forgivingBforgivenBforgetsBforgerBforetoldBforestBforeignBfootBfoolsBfollowedBfolkBfoldsBfoldedBfogsBfoghornBfoamBflyersBfluteBflungB
fluffinessBflowsBflowingBflourishBflopBfloorsBfloodingBfloatsBfloatedBfloatBflirtingBflirtatiousBflippedBflipBflightBflickedBflewBflatBflaresBflamethrowerBflakesBflailingBflagBfixedBfitsBfirmlyB	firefliesBfinishBfinalBfilthBfilmB	filigreesBfileBfigureB	fetishizeBfestivalBfescueBferventBfenceB
felttippedBfellowBfeelineffableBfeedbackBfedBfeathersB	featheredBfeatherBfaxedBfatesBfatBfashionBfascistBfarmBfamiliarBfalselyBfallinBfaithsBfaintB
fabulasticBfabricBeyesightBeyeheadBeyedB
extinguishBextentB	explodingBexplodesB	expandingBexotiqueBexistsB	existenceBexerciseBexampleB
exaltationBexactlyBexactBevidenceBeverythingsBeverlastBethiopiaBetherB	eternallyBeruptsBeruptBermBericsBerasingBeraseBequalBentropyBenslavedBenfoldBenergyBendureBencounteredBencinoB
enchantingBemotionsBemotionallyBemoBeminemBembracesBemberBembarassBelvisBelsesBellosBeliteB
eliminatorBelicitBelevatorBelementsB	elegantlyBelasticBeighteenBegalitéBedgesBedgedBecstaticBecstasyBechoesBechoedBebbBeastBearsBearnBdyouBdyeBdurhamBdunnoBdumpBdudBdrumsetBdrumsBdrummerBdrowningBdroppingBdroningBdriesB	dreamlandBdraperyBdrainsBdrainingBdoyleBdowntheyB
downstairsBdowneyBdovesBdoveBdoughBdoubtsBdotBdosedB	doorknobsBdoorknobB
dominationBdomeBdollarsB	doesn’tBdoeBdoctorsBdocksB	divisibleBdivineBdivideB
distortingBdistinctionB	dissolvesBdissBdisplaceB	disordersBdisorderBdisobeyBdismissBdislocationBdiskBdisintegrateBdisgustBdisgraceB	disgorgerBdisgorgeBdisdainBdiscsBdisasterBdisarrayBdisappearerB	directorsBdirectlyBdipB
diminishedB
dictionaryBdickBdialBdevouredBdevilsBdeteriorationBdetdetachmentB
detachmentBdetachedB	destroyerBdeskBdesiringBdesiresBdesignedBdesignB	desiccateB	describesB
depressionB	depressedBdepictBdependsBdenseBdensBdennisBdeniedBdemonsBdemonBdemandedB	delusionsBdelusionBdelinquentsBdelanceyBdegreeBdegradesBdeformedBdeflateBdefenselessBdeeplyBdecriedBdecodesB	decimatedBdecidedBdecideBdcBdazedBdaytimeBdawnsB	davenportBdarlingBdarkestBdarkenedBdaresBdansBdancedB
dadadadadaBdadBdaBcushyBcurtainsBcurseBcupsBcumBcubesBcrystallizedBcrustBcrumbleBcruisesBcruiseBcrowsBcrowdsB	crossfireBcriminalBcrimesBcreatesBcreatedBcreaseB
creamofwaxBcreamingBcreaksBcrayonBcrawlsBcrawledBcrapBcradleBcracksBcrackingBcrabsBcowboysBcowardBcoveredBcountryBcountedBcoughingBcoughBcostBcosmicB
corruptionB	corrosiveB	corridorsBcorpsesB	corporateBcornersBcornBcoreBcopsBcoozeBcookiesBcookBconversationsBconversationBcontrolsBcontreB
contortingB
containingBconsumptionBconsumesBconspicuousBconsiderBconsciousnessBconquerBconnectionsBcongratulateB	confusingBconfusedB
conflictedB	confidentB
confessingBconeBcomputerBcompromisedB
comprehendBcompetitiveBcommunicationB
commissionB	commandosBcoltBcolouredBcolourBcollideB
collectingBcollectBcoilBcogsBcocksureBcocacolaBcoatsBcoachB	clutchingBclueBcloudyBcloudedBclosetsBcloselyBclosedinBclogsBcloakedBclippenBclipBclimateBclenchedBcleavingBclearmindedBclearerBcleansBclawingBclassicBclappedBclamorBcitadelBcirculationBcircuitsBcirclingBcinemaBciggysBchurchB	christianBchosenBchokeBchoiceB	chocolateBchimingBchillingBchillBchiffonBchidingBchickBchewedBchewBchevyBcherryB	chemistryB
cheesecakeBcheeseBcheckedBchatterBchasmBchaseBcharmsBcharmedB	chargrillBcharadeBchannelsBchangesBchairB	certainlyBcentreBcellsB	celebrateBcatsBcathyBcatBcaseBcarvedBcarryingBcarryBcarrotBcarnalBcargoBcaressedBcaressBcaresB	carefullyB	careeningBcaptureB	captivityB
captivatedBcapsuleBcanyonBcanterburyhollywoodBcansB	canistersBcandlesBcanceledBcameraBcalmBcallsB
californiaBcakesBcakeBcadillacBcBbuttonsB	butterflyBbutterBbuttBbusterBbustedBbustBbusBburstingB	burnishedBburiesBburgerBburdenBbungalowBbumpingBbumperBbummerBbumBbulletsBbuildingBbuddyBbuddhaBbubbleBbruisesBbruisedBbrothersBbroomeBbroodBbrokeBbroadwayB	brightestBbridgeBbreedBbreathedB	breakneckBbreakingB	breakfastBbreakerBbratfinkB
brancafestBboysmashB	boyfriendBboxedBboweryBbouquetBbounceB	boulevardBbougieBbottlesBbottleBborrowedBboringBboreBbordersBboothB	bookstoreBboltBbolderBboilingBblurB	blueskingBbluejeanBblowupBblownBbloomBbloodshadowBblondesBblondeBblocksBblindingBblinderBblightedB	blessingsBblessedBblendsBblendedBbledBbleatBblamBbladeBblackestBblackenBblackedBbitterBbiteBbitchingBbillionBbewmanBbetrayedBbendingBbendedBbeltBbeliefBbeholdB	beginningBbeganBbedtimeBbedlamB	beautificBbeatingBbeatenBbeardsBbeardedBbeamsBbeaconBbeachBbbBbattlesBbathroomBbathBbatchBbatBbastardsBbastardB
basketballBbashfulBbashBbasedBbarkingBbarkBbarestBbarefootBbareBbarbedBbankBbamBballBbagsBbadlyBbackyardBbackwoodB	backseatsB
backgroundBbackedupBbabyoBbaBbBaxesBawokeBawhileBawaytheyBawaitsBavatarBattractBattendBatomicBatlantaB	athinkingB
aswitchingBassuredBassaultBaskedBashleyBasasB	artifactsBarthurB	arroganceBarrivesBariseBarchitecturalBarcBarafatBapuffB
apprenticeB
applesauceBapplauseBanytimeBanymoreBanybodysBantiwarBantiqueBantigodBanthemBansweredBanswerBannihilationBannihilatingBanklesBanitaBangieB	and itsBanarchyBanalogB	amplifierBamongB
amerikkkanBamazingBalthoughBallsBallrightBallowedBalleyBallconsumingBalignedBalertBaleatherBalarmBairsBairless–aBailsBahemBagreeingBagonizinglyBagnesB	afternoonBaffordBaffectBadverseBadultsBadrenalBadmiredBaddsBaddBactsBactionsBactionBactBacomingB	acoastingBachooBachingBachesBacceptBabyssB	abutmentsB	absorbingBabjectB	abdominalBaaaaaaaawwwooooooowwwaaaaaaaoooBaaaaaaaaaaoooowBaaaaaaaaaaaaaaaaaaB9B8B7B40ishB400B4B200B1stB1964B1963B1962B1957B15B14thB120B100B1
??
Const_5Const*
_output_shapes	
:?"*
dtype0	*??
value??B??	?""??                                                 	       
                                                                                                                                                                  !       "       #       $       %       &       '       (       )       *       +       ,       -       .       /       0       1       2       3       4       5       6       7       8       9       :       ;       <       =       >       ?       @       A       B       C       D       E       F       G       H       I       J       K       L       M       N       O       P       Q       R       S       T       U       V       W       X       Y       Z       [       \       ]       ^       _       `       a       b       c       d       e       f       g       h       i       j       k       l       m       n       o       p       q       r       s       t       u       v       w       x       y       z       {       |       }       ~              ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?       ?                                                              	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?       	      	      	      	      	      	      	      	      	      		      
	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	      	       	      !	      "	      #	      $	      %	      &	      '	      (	      )	      *	      +	      ,	      -	      .	      /	      0	      1	      2	      3	      4	      5	      6	      7	      8	      9	      :	      ;	      <	      =	      >	      ?	      @	      A	      B	      C	      D	      E	      F	      G	      H	      I	      J	      K	      L	      M	      N	      O	      P	      Q	      R	      S	      T	      U	      V	      W	      X	      Y	      Z	      [	      \	      ]	      ^	      _	      `	      a	      b	      c	      d	      e	      f	      g	      h	      i	      j	      k	      l	      m	      n	      o	      p	      q	      r	      s	      t	      u	      v	      w	      x	      y	      z	      {	      |	      }	      ~	      	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	      ?	       
      
      
      
      
      
      
      
      
      	
      

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       
      !
      "
      #
      $
      %
      &
      '
      (
      )
      *
      +
      ,
      -
      .
      /
      0
      1
      2
      3
      4
      5
      6
      7
      8
      9
      :
      ;
      <
      =
      >
      ?
      @
      A
      B
      C
      D
      E
      F
      G
      H
      I
      J
      K
      L
      M
      N
      O
      P
      Q
      R
      S
      T
      U
      V
      W
      X
      Y
      Z
      [
      \
      ]
      ^
      _
      `
      a
      b
      c
      d
      e
      f
      g
      h
      i
      j
      k
      l
      m
      n
      o
      p
      q
      r
      s
      t
      u
      v
      w
      x
      y
      z
      {
      |
      }
      ~
      
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      C      D      E      F      G      H      I      J      K      L      M      N      O      P      Q      R      S      T      U      V      W      X      Y      Z      [      \      ]      ^      _      `      a      b      c      d      e      f      g      h      i      j      k      l      m      n      o      p      q      r      s      t      u      v      w      x      y      z      {      |      }      ~            ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?      ?                                                             	      
                                                                                                                                           !      "      #      $      %      &      '      (      )      *      +      ,      -      .      /      0      1      2      3      4      5      6      7      8      9      :      ;      <      =      >      ?      @      A      B      
?
StatefulPartitionedCall_2StatefulPartitionedCallStatefulPartitionedCallConst_4Const_5*
Tin
2	*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *#
fR
__inference_<lambda>_18242
?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *#
fR
__inference_<lambda>_18248
:
NoOpNoOp^PartitionedCall^StatefulPartitionedCall_2
?
3None_lookup_table_export_values/LookupTableExportV2LookupTableExportV2StatefulPartitionedCall_1*
Tkeys0*
Tvalues0	*,
_class"
 loc:@StatefulPartitionedCall_1*
_output_shapes

::
?
Const_6Const"/device:CPU:0*
_output_shapes
: *
dtype0*?
value?B? B?
?
layer-0
layer_with_weights-0
layer-1
layer_with_weights-1
layer-2
layer-3
layer_with_weights-2
layer-4
layer-5
layer_with_weights-3
layer-6
	optimizer
	
signatures
#
_self_saveable_object_factories
	variables
trainable_variables
regularization_losses
	keras_api
%
#_self_saveable_object_factories
M
_index_lookup_layer
#_self_saveable_object_factories
	keras_api
?

embeddings
#_self_saveable_object_factories
	variables
trainable_variables
regularization_losses
	keras_api
w
#_self_saveable_object_factories
	variables
trainable_variables
regularization_losses
	keras_api
?

kernel
bias
# _self_saveable_object_factories
!	variables
"trainable_variables
#regularization_losses
$	keras_api
w
#%_self_saveable_object_factories
&	variables
'trainable_variables
(regularization_losses
)	keras_api
?

*kernel
+bias
#,_self_saveable_object_factories
-	variables
.trainable_variables
/regularization_losses
0	keras_api
 
 
 
#
1
2
3
*4
+5
#
0
1
2
*3
+4
 
?
1layer_metrics
2layer_regularization_losses
	variables
3non_trainable_variables
trainable_variables
4metrics

5layers
regularization_losses
 
X
6lookup_table
7token_counts
#8_self_saveable_object_factories
9	keras_api
 
 
ge
VARIABLE_VALUEembedding_15/embeddings:layer_with_weights-1/embeddings/.ATTRIBUTES/VARIABLE_VALUE
 

0

0
 
?
:layer_metrics
;layer_regularization_losses
	variables
<non_trainable_variables
trainable_variables
=metrics

>layers
regularization_losses
 
 
 
 
?
?layer_metrics
@layer_regularization_losses
	variables
Anon_trainable_variables
trainable_variables
Bmetrics

Clayers
regularization_losses
[Y
VARIABLE_VALUEdense_40/kernel6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUE
WU
VARIABLE_VALUEdense_40/bias4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUE
 

0
1

0
1
 
?
Dlayer_metrics
Elayer_regularization_losses
!	variables
Fnon_trainable_variables
"trainable_variables
Gmetrics

Hlayers
#regularization_losses
 
 
 
 
?
Ilayer_metrics
Jlayer_regularization_losses
&	variables
Knon_trainable_variables
'trainable_variables
Lmetrics

Mlayers
(regularization_losses
[Y
VARIABLE_VALUEdense_39/kernel6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUE
WU
VARIABLE_VALUEdense_39/bias4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUE
 

*0
+1

*0
+1
 
?
Nlayer_metrics
Olayer_regularization_losses
-	variables
Pnon_trainable_variables
.trainable_variables
Qmetrics

Rlayers
/regularization_losses
 
 
 

S0
T1
1
0
1
2
3
4
5
6

U_initializer
RP
tableGlayer_with_weights-0/_index_lookup_layer/token_counts/.ATTRIBUTES/table
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
4
	Vtotal
	Wcount
X	variables
Y	keras_api
D
	Ztotal
	[count
\
_fn_kwargs
]	variables
^	keras_api
 
OM
VARIABLE_VALUEtotal4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE
OM
VARIABLE_VALUEcount4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE

V0
W1

X	variables
QO
VARIABLE_VALUEtotal_14keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUE
QO
VARIABLE_VALUEcount_14keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUE
 

Z0
[1

]	variables
z
serving_default_input_8Placeholder*'
_output_shapes
:?????????*
dtype0*
shape:?????????
?
StatefulPartitionedCall_3StatefulPartitionedCallserving_default_input_8StatefulPartitionedCallConstConst_1Const_2embedding_15/embeddingsdense_40/kerneldense_40/biasdense_39/kerneldense_39/bias*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*-
config_proto

CPU

GPU 2J 8? *,
f'R%
#__inference_signature_wrapper_17336
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
?
StatefulPartitionedCall_4StatefulPartitionedCallsaver_filename+embedding_15/embeddings/Read/ReadVariableOp#dense_40/kernel/Read/ReadVariableOp!dense_40/bias/Read/ReadVariableOp#dense_39/kernel/Read/ReadVariableOp!dense_39/bias/Read/ReadVariableOp3None_lookup_table_export_values/LookupTableExportV25None_lookup_table_export_values/LookupTableExportV2:1total/Read/ReadVariableOpcount/Read/ReadVariableOptotal_1/Read/ReadVariableOpcount_1/Read/ReadVariableOpConst_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *'
f"R 
__inference__traced_save_18320
?
StatefulPartitionedCall_5StatefulPartitionedCallsaver_filenameembedding_15/embeddingsdense_40/kerneldense_40/biasdense_39/kerneldense_39/biasStatefulPartitionedCall_1totalcounttotal_1count_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? **
f%R#
!__inference__traced_restore_18360??
?
8
(__inference_restored_function_body_18199
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *%
f R
__inference__destroyer_153242
PartitionedCall[
IdentityIdentityPartitionedCall:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
?
(__inference_restored_function_body_18151
unknown
	unknown_0
	unknown_1	
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallunknown	unknown_0	unknown_1*
Tin
2	*
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *'
f"R 
__inference__initializer_150902
StatefulPartitionedCallj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*#
_input_shapes
: :?":?"22
StatefulPartitionedCallStatefulPartitionedCall:!

_output_shapes	
:?":!

_output_shapes	
:?"
?
*
__inference_<lambda>_18248
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_181882
PartitionedCallS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
ConstQ
IdentityIdentityConst:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
?
G__inference_embedding_15_layer_call_and_return_conditional_losses_17876

inputs	
inputs_1	A
.embedding_lookup_ragged_embedding_lookup_17869:	?N
identity

identity_1	??(embedding_lookup_ragged/embedding_lookup?
(embedding_lookup_ragged/embedding_lookupResourceGather.embedding_lookup_ragged_embedding_lookup_17869inputs",/job:localhost/replica:0/task:0/device:CPU:0*
Tindices0	*A
_class7
53loc:@embedding_lookup_ragged/embedding_lookup/17869*'
_output_shapes
:?????????*
dtype02*
(embedding_lookup_ragged/embedding_lookup?
1embedding_lookup_ragged/embedding_lookup/IdentityIdentity1embedding_lookup_ragged/embedding_lookup:output:0",/job:localhost/replica:0/task:0/device:CPU:0*
T0*A
_class7
53loc:@embedding_lookup_ragged/embedding_lookup/17869*'
_output_shapes
:?????????23
1embedding_lookup_ragged/embedding_lookup/Identity?
3embedding_lookup_ragged/embedding_lookup/Identity_1Identity:embedding_lookup_ragged/embedding_lookup/Identity:output:0*
T0*'
_output_shapes
:?????????25
3embedding_lookup_ragged/embedding_lookup/Identity_1?
IdentityIdentity<embedding_lookup_ragged/embedding_lookup/Identity_1:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identityc

Identity_1Identityinputs_1^NoOp*
T0	*#
_output_shapes
:?????????2

Identity_1y
NoOpNoOp)^embedding_lookup_ragged/embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*3
_input_shapes"
 :?????????:?????????: 2T
(embedding_lookup_ragged/embedding_lookup(embedding_lookup_ragged/embedding_lookup:K G
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs
?
,
__inference__destroyer_15324
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_153192
PartitionedCallP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
ConstQ
IdentityIdentityConst:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
[
(__inference_restored_function_body_18179
identity: ??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *#
fR
__inference__creator_145682
StatefulPartitionedCallj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 22
StatefulPartitionedCallStatefulPartitionedCall
?
,
__inference__destroyer_18203
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_181992
PartitionedCallP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
ConstQ
IdentityIdentityConst:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
r
__inference_<lambda>_18242
unknown
	unknown_0
	unknown_1	
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallunknown	unknown_0	unknown_1*
Tin
2	*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_181512
StatefulPartitionedCallS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
ConstX
IdentityIdentityConst:output:0^NoOp*
T0*
_output_shapes
: 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*#
_input_shapes
: :?":?"22
StatefulPartitionedCallStatefulPartitionedCall:!

_output_shapes	
:?":!

_output_shapes	
:?"
?
[
(__inference_restored_function_body_14564
identity: ??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *#
fR
__inference__creator_145602
StatefulPartitionedCallh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOpj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 22
StatefulPartitionedCallStatefulPartitionedCall
?
G
__inference__creator_18139
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_181362
StatefulPartitionedCallj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 22
StatefulPartitionedCallStatefulPartitionedCall
?
v
__inference__initializer_18161
unknown
	unknown_0
	unknown_1	
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallunknown	unknown_0	unknown_1*
Tin
2	*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_181512
StatefulPartitionedCallP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
ConstX
IdentityIdentityConst:output:0^NoOp*
T0*
_output_shapes
: 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*#
_input_shapes
: :?":?"22
StatefulPartitionedCallStatefulPartitionedCall:!

_output_shapes	
:?":!

_output_shapes	
:?"
?
r
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_17893

inputs
identityr
Mean/reduction_indicesConst*
_output_shapes
: *
dtype0*
value	B :2
Mean/reduction_indicesx
MeanMeaninputsMean/reduction_indices:output:0*
T0*0
_output_shapes
:??????????????????2
Meanj
IdentityIdentityMean:output:0*
T0*0
_output_shapes
:??????????????????2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*<
_input_shapes+
):'???????????????????????????:e a
=
_output_shapes+
):'???????????????????????????
 
_user_specified_nameinputs
?
F
__inference__creator_14560
identity: ??MutableHashTable?
MutableHashTableMutableHashTableV2*
_output_shapes
: *
	key_dtype0*2
shared_name#!table_136685_load_7982_load_14239*
value_dtype0	2
MutableHashTablea
NoOpNoOp^MutableHashTable*"
_acd_function_control_output(*
_output_shapes
 2
NoOpi
IdentityIdentityMutableHashTable:table_handle:0^NoOp*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 2$
MutableHashTableMutableHashTable
?
r
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_16672

inputs
identityr
Mean/reduction_indicesConst*
_output_shapes
: *
dtype0*
value	B :2
Mean/reduction_indicesx
MeanMeaninputsMean/reduction_indices:output:0*
T0*0
_output_shapes
:??????????????????2
Meanj
IdentityIdentityMean:output:0*
T0*0
_output_shapes
:??????????????????2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*<
_input_shapes+
):'???????????????????????????:e a
=
_output_shapes+
):'???????????????????????????
 
_user_specified_nameinputs
?
.
__inference__initializer_15398
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_153932
PartitionedCallP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
ConstQ
IdentityIdentityConst:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
d
E__inference_dropout_15_layer_call_and_return_conditional_losses_18109

inputs
identity?c
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *   @2
dropout/Consts
dropout/MulMulinputsdropout/Const:output:0*
T0*'
_output_shapes
:?????????@2
dropout/MulT
dropout/ShapeShapeinputs*
T0*
_output_shapes
:2
dropout/Shape?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*'
_output_shapes
:?????????@*
dtype02&
$dropout/random_uniform/RandomUniformu
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *   ?2
dropout/GreaterEqual/y?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:?????????@2
dropout/GreaterEqual
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:?????????@2
dropout/Castz
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*'
_output_shapes
:?????????@2
dropout/Mul_1e
IdentityIdentitydropout/Mul_1:z:0*
T0*'
_output_shapes
:?????????@2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????@:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?
W
;__inference_global_average_pooling1d_15_layer_call_fn_17881

inputs
identity?
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *0
_output_shapes
:??????????????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *_
fZRX
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_166722
PartitionedCallu
IdentityIdentityPartitionedCall:output:0*
T0*0
_output_shapes
:??????????????????2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*<
_input_shapes+
):'???????????????????????????:e a
=
_output_shapes+
):'???????????????????????????
 
_user_specified_nameinputs
?
?
__inference_save_fn_18222
checkpoint_keyD
@none_lookup_table_export_values_lookuptableexportv2_table_handle
identity

identity_1

identity_2

identity_3

identity_4

identity_5	??3None_lookup_table_export_values/LookupTableExportV2?
3None_lookup_table_export_values/LookupTableExportV2LookupTableExportV2@none_lookup_table_export_values_lookuptableexportv2_table_handle",/job:localhost/replica:0/task:0/device:CPU:0*
Tkeys0*
Tvalues0	*
_output_shapes

::25
3None_lookup_table_export_values/LookupTableExportV2T
add/yConst*
_output_shapes
: *
dtype0*
valueB B-keys2
add/yR
addAddcheckpoint_keyadd/y:output:0*
T0*
_output_shapes
: 2
addZ
add_1/yConst*
_output_shapes
: *
dtype0*
valueB B-values2	
add_1/yX
add_1Addcheckpoint_keyadd_1/y:output:0*
T0*
_output_shapes
: 2
add_1Q
IdentityIdentityadd:z:0^NoOp*
T0*
_output_shapes
: 2

IdentityO
ConstConst*
_output_shapes
: *
dtype0*
valueB B 2
Const\

Identity_1IdentityConst:output:0^NoOp*
T0*
_output_shapes
: 2

Identity_1?

Identity_2Identity:None_lookup_table_export_values/LookupTableExportV2:keys:0^NoOp*
T0*
_output_shapes
:2

Identity_2W

Identity_3Identity	add_1:z:0^NoOp*
T0*
_output_shapes
: 2

Identity_3S
Const_1Const*
_output_shapes
: *
dtype0*
valueB B 2	
Const_1^

Identity_4IdentityConst_1:output:0^NoOp*
T0*
_output_shapes
: 2

Identity_4?

Identity_5Identity<None_lookup_table_export_values/LookupTableExportV2:values:0^NoOp*
T0	*
_output_shapes
:2

Identity_5?
NoOpNoOp4^None_lookup_table_export_values/LookupTableExportV2*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0"!

identity_4Identity_4:output:0"!

identity_5Identity_5:output:0*(
_construction_contextkEagerRuntime*
_input_shapes
: : 2j
3None_lookup_table_export_values/LookupTableExportV23None_lookup_table_export_values/LookupTableExportV2:F B

_output_shapes
: 
(
_user_specified_namecheckpoint_key
?
:
__inference__creator_15048
identity??
hash_table?

hash_tableHashTableV2*
_output_shapes
: *
	key_dtype0*1
shared_name" 136813_load_7982_8337_load_14239*
use_node_name_sharing(*
value_dtype0	2

hash_table[
NoOpNoOp^hash_table*"
_acd_function_control_output(*
_output_shapes
 2
NoOpc
IdentityIdentityhash_table:table_handle:0^NoOp*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 2

hash_table
hash_table
??
?
C__inference_model_17_layer_call_and_return_conditional_losses_16966

inputsS
Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handleT
Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value	0
,text_vectorization_5_string_lookup_4_equal_y3
/text_vectorization_5_string_lookup_4_selectv2_t	%
embedding_15_16750:	?N 
dense_40_16936:@
dense_40_16938:@ 
dense_39_16960:@
dense_39_16962:
identity?? dense_39/StatefulPartitionedCall? dense_40/StatefulPartitionedCall?$embedding_15/StatefulPartitionedCall?Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
 text_vectorization_5/StringLowerStringLowerinputs*'
_output_shapes
:?????????2"
 text_vectorization_5/StringLower?
'text_vectorization_5/StaticRegexReplaceStaticRegexReplace)text_vectorization_5/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite 2)
'text_vectorization_5/StaticRegexReplace?
text_vectorization_5/SqueezeSqueeze0text_vectorization_5/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????2
text_vectorization_5/Squeeze?
&text_vectorization_5/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B 2(
&text_vectorization_5/StringSplit/Const?
.text_vectorization_5/StringSplit/StringSplitV2StringSplitV2%text_vectorization_5/Squeeze:output:0/text_vectorization_5/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:20
.text_vectorization_5/StringSplit/StringSplitV2?
4text_vectorization_5/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        26
4text_vectorization_5/StringSplit/strided_slice/stack?
6text_vectorization_5/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       28
6text_vectorization_5/StringSplit/strided_slice/stack_1?
6text_vectorization_5/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      28
6text_vectorization_5/StringSplit/strided_slice/stack_2?
.text_vectorization_5/StringSplit/strided_sliceStridedSlice8text_vectorization_5/StringSplit/StringSplitV2:indices:0=text_vectorization_5/StringSplit/strided_slice/stack:output:0?text_vectorization_5/StringSplit/strided_slice/stack_1:output:0?text_vectorization_5/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask20
.text_vectorization_5/StringSplit/strided_slice?
6text_vectorization_5/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 28
6text_vectorization_5/StringSplit/strided_slice_1/stack?
8text_vectorization_5/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_1?
8text_vectorization_5/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_2?
0text_vectorization_5/StringSplit/strided_slice_1StridedSlice6text_vectorization_5/StringSplit/StringSplitV2:shape:0?text_vectorization_5/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask22
0text_vectorization_5/StringSplit/strided_slice_1?
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_5/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2Y
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_5/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: 2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod?
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : 2g
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2?
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:?????????2f
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum?
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R 2d
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2LookupTableFindV2Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handle7text_vectorization_5/StringSplit/StringSplitV2:values:0Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:?????????2D
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
*text_vectorization_5/string_lookup_4/EqualEqual7text_vectorization_5/StringSplit/StringSplitV2:values:0,text_vectorization_5_string_lookup_4_equal_y*
T0*#
_output_shapes
:?????????2,
*text_vectorization_5/string_lookup_4/Equal?
-text_vectorization_5/string_lookup_4/SelectV2SelectV2.text_vectorization_5/string_lookup_4/Equal:z:0/text_vectorization_5_string_lookup_4_selectv2_tKtext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/SelectV2?
-text_vectorization_5/string_lookup_4/IdentityIdentity6text_vectorization_5/string_lookup_4/SelectV2:output:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/Identity?
$embedding_15/StatefulPartitionedCallStatefulPartitionedCall6text_vectorization_5/string_lookup_4/Identity:output:0btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0embedding_15_16750*
Tin
2		*
Tout
2	*
_collective_manager_ids
 *6
_output_shapes$
":?????????:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_15_layer_call_and_return_conditional_losses_167492&
$embedding_15/StatefulPartitionedCall?
+global_average_pooling1d_15/PartitionedCallPartitionedCall-embedding_15/StatefulPartitionedCall:output:0-embedding_15/StatefulPartitionedCall:output:1*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *_
fZRX
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_169222-
+global_average_pooling1d_15/PartitionedCall?
 dense_40/StatefulPartitionedCallStatefulPartitionedCall4global_average_pooling1d_15/PartitionedCall:output:0dense_40_16936dense_40_16938*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_40_layer_call_and_return_conditional_losses_169352"
 dense_40/StatefulPartitionedCall?
dropout_15/PartitionedCallPartitionedCall)dense_40/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_15_layer_call_and_return_conditional_losses_169462
dropout_15/PartitionedCall?
 dense_39/StatefulPartitionedCallStatefulPartitionedCall#dropout_15/PartitionedCall:output:0dense_39_16960dense_39_16962*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_39_layer_call_and_return_conditional_losses_169592"
 dense_39/StatefulPartitionedCall?
IdentityIdentity)dense_39/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity?
NoOpNoOp!^dense_39/StatefulPartitionedCall!^dense_40/StatefulPartitionedCall%^embedding_15/StatefulPartitionedCallC^text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2D
 dense_39/StatefulPartitionedCall dense_39/StatefulPartitionedCall2D
 dense_40/StatefulPartitionedCall dense_40/StatefulPartitionedCall2L
$embedding_15/StatefulPartitionedCall$embedding_15/StatefulPartitionedCall2?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
?
G__inference_embedding_15_layer_call_and_return_conditional_losses_16749

inputs	
inputs_1	A
.embedding_lookup_ragged_embedding_lookup_16742:	?N
identity

identity_1	??(embedding_lookup_ragged/embedding_lookup?
(embedding_lookup_ragged/embedding_lookupResourceGather.embedding_lookup_ragged_embedding_lookup_16742inputs",/job:localhost/replica:0/task:0/device:CPU:0*
Tindices0	*A
_class7
53loc:@embedding_lookup_ragged/embedding_lookup/16742*'
_output_shapes
:?????????*
dtype02*
(embedding_lookup_ragged/embedding_lookup?
1embedding_lookup_ragged/embedding_lookup/IdentityIdentity1embedding_lookup_ragged/embedding_lookup:output:0",/job:localhost/replica:0/task:0/device:CPU:0*
T0*A
_class7
53loc:@embedding_lookup_ragged/embedding_lookup/16742*'
_output_shapes
:?????????23
1embedding_lookup_ragged/embedding_lookup/Identity?
3embedding_lookup_ragged/embedding_lookup/Identity_1Identity:embedding_lookup_ragged/embedding_lookup/Identity:output:0*
T0*'
_output_shapes
:?????????25
3embedding_lookup_ragged/embedding_lookup/Identity_1?
IdentityIdentity<embedding_lookup_ragged/embedding_lookup/Identity_1:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identityc

Identity_1Identityinputs_1^NoOp*
T0	*#
_output_shapes
:?????????2

Identity_1y
NoOpNoOp)^embedding_lookup_ragged/embedding_lookup*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*3
_input_shapes"
 :?????????:?????????: 2T
(embedding_lookup_ragged/embedding_lookup(embedding_lookup_ragged/embedding_lookup:K G
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs
?
8
(__inference_restored_function_body_15393
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *'
f"R 
__inference__initializer_153892
PartitionedCall[
IdentityIdentityPartitionedCall:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
U
(__inference_restored_function_body_15052
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *#
fR
__inference__creator_150482
StatefulPartitionedCallh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOpj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 22
StatefulPartitionedCallStatefulPartitionedCall
?

?
#__inference_signature_wrapper_17336
input_8
unknown
	unknown_0	
	unknown_1
	unknown_2	
	unknown_3:	?N
	unknown_4:@
	unknown_5:@
	unknown_6:@
	unknown_7:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinput_8unknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*-
config_proto

CPU

GPU 2J 8? *)
f$R"
 __inference__wrapped_model_166622
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_8:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
??
?
C__inference_model_17_layer_call_and_return_conditional_losses_17137

inputsS
Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handleT
Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value	0
,text_vectorization_5_string_lookup_4_equal_y3
/text_vectorization_5_string_lookup_4_selectv2_t	%
embedding_15_17120:	?N 
dense_40_17125:@
dense_40_17127:@ 
dense_39_17131:@
dense_39_17133:
identity?? dense_39/StatefulPartitionedCall? dense_40/StatefulPartitionedCall?"dropout_15/StatefulPartitionedCall?$embedding_15/StatefulPartitionedCall?Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
 text_vectorization_5/StringLowerStringLowerinputs*'
_output_shapes
:?????????2"
 text_vectorization_5/StringLower?
'text_vectorization_5/StaticRegexReplaceStaticRegexReplace)text_vectorization_5/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite 2)
'text_vectorization_5/StaticRegexReplace?
text_vectorization_5/SqueezeSqueeze0text_vectorization_5/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????2
text_vectorization_5/Squeeze?
&text_vectorization_5/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B 2(
&text_vectorization_5/StringSplit/Const?
.text_vectorization_5/StringSplit/StringSplitV2StringSplitV2%text_vectorization_5/Squeeze:output:0/text_vectorization_5/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:20
.text_vectorization_5/StringSplit/StringSplitV2?
4text_vectorization_5/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        26
4text_vectorization_5/StringSplit/strided_slice/stack?
6text_vectorization_5/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       28
6text_vectorization_5/StringSplit/strided_slice/stack_1?
6text_vectorization_5/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      28
6text_vectorization_5/StringSplit/strided_slice/stack_2?
.text_vectorization_5/StringSplit/strided_sliceStridedSlice8text_vectorization_5/StringSplit/StringSplitV2:indices:0=text_vectorization_5/StringSplit/strided_slice/stack:output:0?text_vectorization_5/StringSplit/strided_slice/stack_1:output:0?text_vectorization_5/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask20
.text_vectorization_5/StringSplit/strided_slice?
6text_vectorization_5/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 28
6text_vectorization_5/StringSplit/strided_slice_1/stack?
8text_vectorization_5/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_1?
8text_vectorization_5/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_2?
0text_vectorization_5/StringSplit/strided_slice_1StridedSlice6text_vectorization_5/StringSplit/StringSplitV2:shape:0?text_vectorization_5/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask22
0text_vectorization_5/StringSplit/strided_slice_1?
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_5/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2Y
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_5/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: 2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod?
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : 2g
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2?
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:?????????2f
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum?
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R 2d
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2LookupTableFindV2Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handle7text_vectorization_5/StringSplit/StringSplitV2:values:0Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:?????????2D
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
*text_vectorization_5/string_lookup_4/EqualEqual7text_vectorization_5/StringSplit/StringSplitV2:values:0,text_vectorization_5_string_lookup_4_equal_y*
T0*#
_output_shapes
:?????????2,
*text_vectorization_5/string_lookup_4/Equal?
-text_vectorization_5/string_lookup_4/SelectV2SelectV2.text_vectorization_5/string_lookup_4/Equal:z:0/text_vectorization_5_string_lookup_4_selectv2_tKtext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/SelectV2?
-text_vectorization_5/string_lookup_4/IdentityIdentity6text_vectorization_5/string_lookup_4/SelectV2:output:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/Identity?
$embedding_15/StatefulPartitionedCallStatefulPartitionedCall6text_vectorization_5/string_lookup_4/Identity:output:0btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0embedding_15_17120*
Tin
2		*
Tout
2	*
_collective_manager_ids
 *6
_output_shapes$
":?????????:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_15_layer_call_and_return_conditional_losses_167492&
$embedding_15/StatefulPartitionedCall?
+global_average_pooling1d_15/PartitionedCallPartitionedCall-embedding_15/StatefulPartitionedCall:output:0-embedding_15/StatefulPartitionedCall:output:1*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *_
fZRX
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_169222-
+global_average_pooling1d_15/PartitionedCall?
 dense_40/StatefulPartitionedCallStatefulPartitionedCall4global_average_pooling1d_15/PartitionedCall:output:0dense_40_17125dense_40_17127*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_40_layer_call_and_return_conditional_losses_169352"
 dense_40/StatefulPartitionedCall?
"dropout_15/StatefulPartitionedCallStatefulPartitionedCall)dense_40/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_15_layer_call_and_return_conditional_losses_170172$
"dropout_15/StatefulPartitionedCall?
 dense_39/StatefulPartitionedCallStatefulPartitionedCall+dropout_15/StatefulPartitionedCall:output:0dense_39_17131dense_39_17133*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_39_layer_call_and_return_conditional_losses_169592"
 dense_39/StatefulPartitionedCall?
IdentityIdentity)dense_39/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity?
NoOpNoOp!^dense_39/StatefulPartitionedCall!^dense_40/StatefulPartitionedCall#^dropout_15/StatefulPartitionedCall%^embedding_15/StatefulPartitionedCallC^text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2D
 dense_39/StatefulPartitionedCall dense_39/StatefulPartitionedCall2D
 dense_40/StatefulPartitionedCall dense_40/StatefulPartitionedCall2H
"dropout_15/StatefulPartitionedCall"dropout_15/StatefulPartitionedCall2L
$embedding_15/StatefulPartitionedCall$embedding_15/StatefulPartitionedCall2?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
v
__inference__initializer_15090
unknown
	unknown_0
	unknown_1	
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallunknown	unknown_0	unknown_1*
Tin
2	*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_150742
StatefulPartitionedCallP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Consth
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOpX
IdentityIdentityConst:output:0^NoOp*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*#
_input_shapes
: :?":?"22
StatefulPartitionedCallStatefulPartitionedCall:!

_output_shapes	
:?":!

_output_shapes	
:?"
?
8
(__inference_restored_function_body_15319
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *%
f R
__inference__destroyer_153152
PartitionedCall[
IdentityIdentityPartitionedCall:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
d
E__inference_dropout_15_layer_call_and_return_conditional_losses_17017

inputs
identity?c
dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *   @2
dropout/Consts
dropout/MulMulinputsdropout/Const:output:0*
T0*'
_output_shapes
:?????????@2
dropout/MulT
dropout/ShapeShapeinputs*
T0*
_output_shapes
:2
dropout/Shape?
$dropout/random_uniform/RandomUniformRandomUniformdropout/Shape:output:0*
T0*'
_output_shapes
:?????????@*
dtype02&
$dropout/random_uniform/RandomUniformu
dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *   ?2
dropout/GreaterEqual/y?
dropout/GreaterEqualGreaterEqual-dropout/random_uniform/RandomUniform:output:0dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:?????????@2
dropout/GreaterEqual
dropout/CastCastdropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:?????????@2
dropout/Castz
dropout/Mul_1Muldropout/Mul:z:0dropout/Cast:y:0*
T0*'
_output_shapes
:?????????@2
dropout/Mul_1e
IdentityIdentitydropout/Mul_1:z:0*
T0*'
_output_shapes
:?????????@2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????@:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?	
?
__inference_restore_fn_18230
restored_tensors_0
restored_tensors_1	C
?mutablehashtable_table_restore_lookuptableimportv2_table_handle
identity??2MutableHashTable_table_restore/LookupTableImportV2?
2MutableHashTable_table_restore/LookupTableImportV2LookupTableImportV2?mutablehashtable_table_restore_lookuptableimportv2_table_handlerestored_tensors_0restored_tensors_1",/job:localhost/replica:0/task:0/device:CPU:0*	
Tin0*

Tout0	*
_output_shapes
 24
2MutableHashTable_table_restore/LookupTableImportV2P
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
ConstX
IdentityIdentityConst:output:0^NoOp*
T0*
_output_shapes
: 2

Identity?
NoOpNoOp3^MutableHashTable_table_restore/LookupTableImportV2*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes

::: 2h
2MutableHashTable_table_restore/LookupTableImportV22MutableHashTable_table_restore/LookupTableImportV2:L H

_output_shapes
:
,
_user_specified_namerestored_tensors_0:LH

_output_shapes
:
,
_user_specified_namerestored_tensors_1
?

?
(__inference_model_17_layer_call_fn_17181
input_8
unknown
	unknown_0	
	unknown_1
	unknown_2	
	unknown_3:	?N
	unknown_4:@
	unknown_5:@
	unknown_6:@
	unknown_7:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinput_8unknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_model_17_layer_call_and_return_conditional_losses_171372
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_8:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
,
__inference__destroyer_15315
identityP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
ConstQ
IdentityIdentityConst:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
c
E__inference_dropout_15_layer_call_and_return_conditional_losses_18097

inputs

identity_1Z
IdentityIdentityinputs*
T0*'
_output_shapes
:?????????@2

Identityi

Identity_1IdentityIdentity:output:0*
T0*'
_output_shapes
:?????????@2

Identity_1"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????@:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?

?
(__inference_model_17_layer_call_fn_17382

inputs
unknown
	unknown_0	
	unknown_1
	unknown_2	
	unknown_3:	?N
	unknown_4:@
	unknown_5:@
	unknown_6:@
	unknown_7:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_model_17_layer_call_and_return_conditional_losses_171372
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
,
__inference__destroyer_18172
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_181682
PartitionedCallP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
ConstQ
IdentityIdentityConst:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
c
*__inference_dropout_15_layer_call_fn_18092

inputs
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_15_layer_call_and_return_conditional_losses_170172
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????@2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????@22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?
U
(__inference_restored_function_body_18263
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *#
fR
__inference__creator_150562
StatefulPartitionedCallj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*
_input_shapes 22
StatefulPartitionedCallStatefulPartitionedCall
?
8
(__inference_restored_function_body_15021
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *%
f R
__inference__destroyer_150172
PartitionedCall[
IdentityIdentityPartitionedCall:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
.
__inference__initializer_18192
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_181882
PartitionedCallP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
ConstQ
IdentityIdentityConst:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
U
(__inference_restored_function_body_18136
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *#
fR
__inference__creator_150562
StatefulPartitionedCallj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 22
StatefulPartitionedCallStatefulPartitionedCall
?
M
__inference__creator_18182
identity: ??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_181792
StatefulPartitionedCallj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 22
StatefulPartitionedCallStatefulPartitionedCall
?
?
(__inference_dense_39_layer_call_fn_18118

inputs
unknown:@
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_39_layer_call_and_return_conditional_losses_169592
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????@: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
??
?
C__inference_model_17_layer_call_and_return_conditional_losses_17855

inputsS
Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handleT
Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value	0
,text_vectorization_5_string_lookup_4_equal_y3
/text_vectorization_5_string_lookup_4_selectv2_t	N
;embedding_15_embedding_lookup_ragged_embedding_lookup_17663:	?N9
'dense_40_matmul_readvariableop_resource:@6
(dense_40_biasadd_readvariableop_resource:@9
'dense_39_matmul_readvariableop_resource:@6
(dense_39_biasadd_readvariableop_resource:
identity??dense_39/BiasAdd/ReadVariableOp?dense_39/MatMul/ReadVariableOp?dense_40/BiasAdd/ReadVariableOp?dense_40/MatMul/ReadVariableOp?5embedding_15/embedding_lookup_ragged/embedding_lookup?Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
 text_vectorization_5/StringLowerStringLowerinputs*'
_output_shapes
:?????????2"
 text_vectorization_5/StringLower?
'text_vectorization_5/StaticRegexReplaceStaticRegexReplace)text_vectorization_5/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite 2)
'text_vectorization_5/StaticRegexReplace?
text_vectorization_5/SqueezeSqueeze0text_vectorization_5/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????2
text_vectorization_5/Squeeze?
&text_vectorization_5/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B 2(
&text_vectorization_5/StringSplit/Const?
.text_vectorization_5/StringSplit/StringSplitV2StringSplitV2%text_vectorization_5/Squeeze:output:0/text_vectorization_5/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:20
.text_vectorization_5/StringSplit/StringSplitV2?
4text_vectorization_5/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        26
4text_vectorization_5/StringSplit/strided_slice/stack?
6text_vectorization_5/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       28
6text_vectorization_5/StringSplit/strided_slice/stack_1?
6text_vectorization_5/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      28
6text_vectorization_5/StringSplit/strided_slice/stack_2?
.text_vectorization_5/StringSplit/strided_sliceStridedSlice8text_vectorization_5/StringSplit/StringSplitV2:indices:0=text_vectorization_5/StringSplit/strided_slice/stack:output:0?text_vectorization_5/StringSplit/strided_slice/stack_1:output:0?text_vectorization_5/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask20
.text_vectorization_5/StringSplit/strided_slice?
6text_vectorization_5/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 28
6text_vectorization_5/StringSplit/strided_slice_1/stack?
8text_vectorization_5/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_1?
8text_vectorization_5/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_2?
0text_vectorization_5/StringSplit/strided_slice_1StridedSlice6text_vectorization_5/StringSplit/StringSplitV2:shape:0?text_vectorization_5/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask22
0text_vectorization_5/StringSplit/strided_slice_1?
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_5/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2Y
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_5/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: 2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod?
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : 2g
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2?
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:?????????2f
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum?
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R 2d
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2LookupTableFindV2Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handle7text_vectorization_5/StringSplit/StringSplitV2:values:0Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:?????????2D
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
*text_vectorization_5/string_lookup_4/EqualEqual7text_vectorization_5/StringSplit/StringSplitV2:values:0,text_vectorization_5_string_lookup_4_equal_y*
T0*#
_output_shapes
:?????????2,
*text_vectorization_5/string_lookup_4/Equal?
-text_vectorization_5/string_lookup_4/SelectV2SelectV2.text_vectorization_5/string_lookup_4/Equal:z:0/text_vectorization_5_string_lookup_4_selectv2_tKtext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/SelectV2?
-text_vectorization_5/string_lookup_4/IdentityIdentity6text_vectorization_5/string_lookup_4/SelectV2:output:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/Identity?
5embedding_15/embedding_lookup_ragged/embedding_lookupResourceGather;embedding_15_embedding_lookup_ragged_embedding_lookup_176636text_vectorization_5/string_lookup_4/Identity:output:0",/job:localhost/replica:0/task:0/device:CPU:0*
Tindices0	*N
_classD
B@loc:@embedding_15/embedding_lookup_ragged/embedding_lookup/17663*'
_output_shapes
:?????????*
dtype027
5embedding_15/embedding_lookup_ragged/embedding_lookup?
>embedding_15/embedding_lookup_ragged/embedding_lookup/IdentityIdentity>embedding_15/embedding_lookup_ragged/embedding_lookup:output:0",/job:localhost/replica:0/task:0/device:CPU:0*
T0*N
_classD
B@loc:@embedding_15/embedding_lookup_ragged/embedding_lookup/17663*'
_output_shapes
:?????????2@
>embedding_15/embedding_lookup_ragged/embedding_lookup/Identity?
@embedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1IdentityGembedding_15/embedding_lookup_ragged/embedding_lookup/Identity:output:0*
T0*'
_output_shapes
:?????????2B
@embedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1?
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/ShapeShapebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:2D
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/Shape?
Pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2R
Pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack?
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2T
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1?
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2T
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2?
Jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_sliceStridedSliceKglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/Shape:output:0Yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack:output:0[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1:output:0[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2L
Jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice?
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub/yConst*
_output_shapes
: *
dtype0*
value	B :2D
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub/y?
@global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/subSubSglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice:output:0Kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub/y:output:0*
T0*
_output_shapes
: 2B
@global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub?
iglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2k
iglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2?
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_sliceStridedSlicebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*
end_mask2e
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:
?????????2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2?
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1StridedSlicebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask2g
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1?
Yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/subSublglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice:output:0nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1:output:0*
T0	*#
_output_shapes
:?????????2[
Yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub?
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/ShapeShapebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:*
out_type0	2]
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Shape?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2?
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2StridedSlicedglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Shape:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2g
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2?
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/yConst*
_output_shapes
: *
dtype0	*
value	B	 R2_
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/y?
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1Subnglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2:output:0fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/y:output:0*
T0	*
_output_shapes
: 2]
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1?
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/startConst*
_output_shapes
: *
dtype0*
value	B : 2c
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/start?
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/deltaConst*
_output_shapes
: *
dtype0*
value	B :2c
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/delta?
`global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/CastCastjglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/start:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2b
`global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast?
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1Castjglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/delta:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2d
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1?
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/rangeRangedglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast:y:0_global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1:z:0fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1:y:0*

Tidx0	*#
_output_shapes
:?????????2]
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range?
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/CastCast]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub:z:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2c
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Cast?
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ShapeShapedglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range:output:0*
T0	*
_output_shapes
:2d
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Shape?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack?
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2t
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1?
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2t
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2?
jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_sliceStridedSlicekglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Shape:output:0yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack:output:0{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1:output:0{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2l
jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice?
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shapePacksglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice:output:0*
N*
T0*
_output_shapes
:2p
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape?
hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastToBroadcastToeglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Cast:y:0wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape:output:0*
T0*#
_output_shapes
:?????????2j
hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo?
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2d
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Const?
`global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/MaxMaxqglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Const:output:0*
T0*
_output_shapes
: 2b
`global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Max?
fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/xConst*
_output_shapes
: *
dtype0*
value	B : 2h
fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/x?
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/MaximumMaximumoglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/x:output:0iglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Max:output:0*
T0*
_output_shapes
: 2f
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ConstConst*
_output_shapes
: *
dtype0*
value	B : 2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1Const*
_output_shapes
: *
dtype0*
value	B :2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/RangeRangexglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const:output:0hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1:output:0*#
_output_shapes
:?????????2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range?
xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2z
xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim?
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims
ExpandDimsqglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2v
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims?
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/CastCast}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims:output:0*

DstT0*

SrcT0*'
_output_shapes
:?????????2p
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast?
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/LessLessxglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range:output:0rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast:y:0*
T0*0
_output_shapes
:??????????????????2p
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B :2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim?
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims
ExpandDimsdglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim:output:0*
T0	*'
_output_shapes
:?????????2i
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0Const*
_output_shapes
: *
dtype0*
value	B :2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiplesPackvglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0:output:0hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0*
N*
T0*
_output_shapes
:2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples?
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/TileTilepglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples:output:0*
T0	*0
_output_shapes
:??????????????????2c
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ShapeShapejglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape?
}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2?
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_sliceStridedSlicexglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
:2y
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices?
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ProdProd?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices:output:0*
T0*
_output_shapes
: 2p
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1Shapejglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1StridedSlicezglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2Shapejglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2StridedSlicezglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1Packwglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod:output:0*
N*
T0*
_output_shapes
:2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1?
uglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2w
uglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concatConcatV2?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2:output:0~global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis:output:0*
N*
T0*
_output_shapes
:2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ReshapeReshapejglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat:output:0*
T0	*#
_output_shapes
:?????????2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*
valueB:
?????????2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1Reshaperglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less:z:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape:output:0*
T0
*#
_output_shapes
:?????????2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/WhereWhere|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1:output:0*'
_output_shapes
:?????????2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/SqueezeSqueezewglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where:index:0*
T0	*#
_output_shapes
:?????????*
squeeze_dims
2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze?
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axisConst*
_output_shapes
: *
dtype0*
value	B : 2y
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis?
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2GatherV2zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape:output:0zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis:output:0*
Taxis0*
Tindices0	*
Tparams0	*#
_output_shapes
:?????????2t
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2?
Oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSumUnsortedSegmentSumIembedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1:output:0{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2:output:0Dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub:z:0*
T0*
Tindices0	*'
_output_shapes
:?????????2Q
Oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSum?
<global_average_pooling1d_15/RaggedReduceMean/ones_like/ShapeShapeIembedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1:output:0*
T0*
_output_shapes
:2>
<global_average_pooling1d_15/RaggedReduceMean/ones_like/Shape?
<global_average_pooling1d_15/RaggedReduceMean/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2>
<global_average_pooling1d_15/RaggedReduceMean/ones_like/Const?
6global_average_pooling1d_15/RaggedReduceMean/ones_likeFillEglobal_average_pooling1d_15/RaggedReduceMean/ones_like/Shape:output:0Eglobal_average_pooling1d_15/RaggedReduceMean/ones_like/Const:output:0*
T0*'
_output_shapes
:?????????28
6global_average_pooling1d_15/RaggedReduceMean/ones_like?
Dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/ShapeShapebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:2F
Dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/Shape?
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2T
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack?
Tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2V
Tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1?
Tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2V
Tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2?
Lglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_sliceStridedSliceMglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/Shape:output:0[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack:output:0]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1:output:0]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2N
Lglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice?
Dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub/yConst*
_output_shapes
: *
dtype0*
value	B :2F
Dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub/y?
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/subSubUglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice:output:0Mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub/y:output:0*
T0*
_output_shapes
: 2D
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2?
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_sliceStridedSlicebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*
end_mask2g
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:
?????????2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2?
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1StridedSlicebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack:output:0xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1:output:0xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask2i
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1?
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/subSubnglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice:output:0pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1:output:0*
T0	*#
_output_shapes
:?????????2]
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub?
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/ShapeShapebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:*
out_type0	2_
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Shape?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2?
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2StridedSlicefglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Shape:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack:output:0xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1:output:0xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2i
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2?
_global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/yConst*
_output_shapes
: *
dtype0	*
value	B	 R2a
_global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/y?
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1Subpglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2:output:0hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/y:output:0*
T0	*
_output_shapes
: 2_
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1?
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/startConst*
_output_shapes
: *
dtype0*
value	B : 2e
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/start?
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/deltaConst*
_output_shapes
: *
dtype0*
value	B :2e
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/delta?
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/CastCastlglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/start:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2d
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast?
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1Castlglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/delta:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2f
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1?
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/rangeRangefglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast:y:0aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1:z:0hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1:y:0*

Tidx0	*#
_output_shapes
:?????????2_
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range?
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/CastCast_global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub:z:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2e
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Cast?
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ShapeShapefglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range:output:0*
T0	*
_output_shapes
:2f
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Shape?
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2t
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack?
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2v
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1?
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2v
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2?
lglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_sliceStridedSlicemglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Shape:output:0{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack:output:0}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1:output:0}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2n
lglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shapePackuglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice:output:0*
N*
T0*
_output_shapes
:2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape?
jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastToBroadcastTogglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Cast:y:0yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape:output:0*
T0*#
_output_shapes
:?????????2l
jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo?
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2f
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Const?
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/MaxMaxsglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Const:output:0*
T0*
_output_shapes
: 2d
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Max?
hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/xConst*
_output_shapes
: *
dtype0*
value	B : 2j
hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/x?
fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/MaximumMaximumqglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/x:output:0kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Max:output:0*
T0*
_output_shapes
: 2h
fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ConstConst*
_output_shapes
: *
dtype0*
value	B : 2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1Const*
_output_shapes
: *
dtype0*
value	B :2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/RangeRangezglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const:output:0jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1:output:0*#
_output_shapes
:?????????2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range?
zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2|
zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim?
vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims
ExpandDimssglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2x
vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/CastCastglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims:output:0*

DstT0*

SrcT0*'
_output_shapes
:?????????2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/LessLesszglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast:y:0*
T0*0
_output_shapes
:??????????????????2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B :2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim?
iglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims
ExpandDimsfglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim:output:0*
T0	*'
_output_shapes
:?????????2k
iglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0Const*
_output_shapes
: *
dtype0*
value	B :2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiplesPackxglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0:output:0jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0*
N*
T0*
_output_shapes
:2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples?
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/TileTilerglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples:output:0*
T0	*0
_output_shapes
:??????????????????2e
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ShapeShapelglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_sliceStridedSlicezglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
:2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ProdProd?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices:output:0*
T0*
_output_shapes
: 2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1Shapelglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2?
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1StridedSlice|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2}
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2Shapelglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2?
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2StridedSlice|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2}
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2?
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1Packyglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod:output:0*
N*
T0*
_output_shapes
:2}
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1?
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2y
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis?
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concatConcatV2?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis:output:0*
N*
T0*
_output_shapes
:2t
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ReshapeReshapelglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat:output:0*
T0	*#
_output_shapes
:?????????2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape?
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*
valueB:
?????????2}
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape?
uglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1Reshapetglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less:z:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape:output:0*
T0
*#
_output_shapes
:?????????2w
uglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/WhereWhere~global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1:output:0*'
_output_shapes
:?????????2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/SqueezeSqueezeyglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where:index:0*
T0	*#
_output_shapes
:?????????*
squeeze_dims
2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axisConst*
_output_shapes
: *
dtype0*
value	B : 2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis?
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2GatherV2|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape:output:0|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis:output:0*
Taxis0*
Tindices0	*
Tparams0	*#
_output_shapes
:?????????2v
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2?
Qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSumUnsortedSegmentSum?global_average_pooling1d_15/RaggedReduceMean/ones_like:output:0}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2:output:0Fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub:z:0*
T0*
Tindices0	*'
_output_shapes
:?????????2S
Qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSum?
4global_average_pooling1d_15/RaggedReduceMean/truedivRealDivXglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSum:output:0Zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSum:output:0*
T0*'
_output_shapes
:?????????26
4global_average_pooling1d_15/RaggedReduceMean/truediv?
dense_40/MatMul/ReadVariableOpReadVariableOp'dense_40_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02 
dense_40/MatMul/ReadVariableOp?
dense_40/MatMulMatMul8global_average_pooling1d_15/RaggedReduceMean/truediv:z:0&dense_40/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_40/MatMul?
dense_40/BiasAdd/ReadVariableOpReadVariableOp(dense_40_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype02!
dense_40/BiasAdd/ReadVariableOp?
dense_40/BiasAddBiasAdddense_40/MatMul:product:0'dense_40/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_40/BiasAdds
dense_40/ReluReludense_40/BiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
dense_40/Reluy
dropout_15/dropout/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *   @2
dropout_15/dropout/Const?
dropout_15/dropout/MulMuldense_40/Relu:activations:0!dropout_15/dropout/Const:output:0*
T0*'
_output_shapes
:?????????@2
dropout_15/dropout/Mul
dropout_15/dropout/ShapeShapedense_40/Relu:activations:0*
T0*
_output_shapes
:2
dropout_15/dropout/Shape?
/dropout_15/dropout/random_uniform/RandomUniformRandomUniform!dropout_15/dropout/Shape:output:0*
T0*'
_output_shapes
:?????????@*
dtype021
/dropout_15/dropout/random_uniform/RandomUniform?
!dropout_15/dropout/GreaterEqual/yConst*
_output_shapes
: *
dtype0*
valueB
 *   ?2#
!dropout_15/dropout/GreaterEqual/y?
dropout_15/dropout/GreaterEqualGreaterEqual8dropout_15/dropout/random_uniform/RandomUniform:output:0*dropout_15/dropout/GreaterEqual/y:output:0*
T0*'
_output_shapes
:?????????@2!
dropout_15/dropout/GreaterEqual?
dropout_15/dropout/CastCast#dropout_15/dropout/GreaterEqual:z:0*

DstT0*

SrcT0
*'
_output_shapes
:?????????@2
dropout_15/dropout/Cast?
dropout_15/dropout/Mul_1Muldropout_15/dropout/Mul:z:0dropout_15/dropout/Cast:y:0*
T0*'
_output_shapes
:?????????@2
dropout_15/dropout/Mul_1?
dense_39/MatMul/ReadVariableOpReadVariableOp'dense_39_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02 
dense_39/MatMul/ReadVariableOp?
dense_39/MatMulMatMuldropout_15/dropout/Mul_1:z:0&dense_39/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_39/MatMul?
dense_39/BiasAdd/ReadVariableOpReadVariableOp(dense_39_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02!
dense_39/BiasAdd/ReadVariableOp?
dense_39/BiasAddBiasAdddense_39/MatMul:product:0'dense_39/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_39/BiasAdd|
dense_39/SigmoidSigmoiddense_39/BiasAdd:output:0*
T0*'
_output_shapes
:?????????2
dense_39/Sigmoido
IdentityIdentitydense_39/Sigmoid:y:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity?
NoOpNoOp ^dense_39/BiasAdd/ReadVariableOp^dense_39/MatMul/ReadVariableOp ^dense_40/BiasAdd/ReadVariableOp^dense_40/MatMul/ReadVariableOp6^embedding_15/embedding_lookup_ragged/embedding_lookupC^text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2B
dense_39/BiasAdd/ReadVariableOpdense_39/BiasAdd/ReadVariableOp2@
dense_39/MatMul/ReadVariableOpdense_39/MatMul/ReadVariableOp2B
dense_40/BiasAdd/ReadVariableOpdense_40/BiasAdd/ReadVariableOp2@
dense_40/MatMul/ReadVariableOpdense_40/MatMul/ReadVariableOp2n
5embedding_15/embedding_lookup_ragged/embedding_lookup5embedding_15/embedding_lookup_ragged/embedding_lookup2?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
??
?
C__inference_model_17_layer_call_and_return_conditional_losses_17246
input_8S
Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handleT
Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value	0
,text_vectorization_5_string_lookup_4_equal_y3
/text_vectorization_5_string_lookup_4_selectv2_t	%
embedding_15_17229:	?N 
dense_40_17234:@
dense_40_17236:@ 
dense_39_17240:@
dense_39_17242:
identity?? dense_39/StatefulPartitionedCall? dense_40/StatefulPartitionedCall?$embedding_15/StatefulPartitionedCall?Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
 text_vectorization_5/StringLowerStringLowerinput_8*'
_output_shapes
:?????????2"
 text_vectorization_5/StringLower?
'text_vectorization_5/StaticRegexReplaceStaticRegexReplace)text_vectorization_5/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite 2)
'text_vectorization_5/StaticRegexReplace?
text_vectorization_5/SqueezeSqueeze0text_vectorization_5/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????2
text_vectorization_5/Squeeze?
&text_vectorization_5/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B 2(
&text_vectorization_5/StringSplit/Const?
.text_vectorization_5/StringSplit/StringSplitV2StringSplitV2%text_vectorization_5/Squeeze:output:0/text_vectorization_5/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:20
.text_vectorization_5/StringSplit/StringSplitV2?
4text_vectorization_5/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        26
4text_vectorization_5/StringSplit/strided_slice/stack?
6text_vectorization_5/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       28
6text_vectorization_5/StringSplit/strided_slice/stack_1?
6text_vectorization_5/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      28
6text_vectorization_5/StringSplit/strided_slice/stack_2?
.text_vectorization_5/StringSplit/strided_sliceStridedSlice8text_vectorization_5/StringSplit/StringSplitV2:indices:0=text_vectorization_5/StringSplit/strided_slice/stack:output:0?text_vectorization_5/StringSplit/strided_slice/stack_1:output:0?text_vectorization_5/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask20
.text_vectorization_5/StringSplit/strided_slice?
6text_vectorization_5/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 28
6text_vectorization_5/StringSplit/strided_slice_1/stack?
8text_vectorization_5/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_1?
8text_vectorization_5/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_2?
0text_vectorization_5/StringSplit/strided_slice_1StridedSlice6text_vectorization_5/StringSplit/StringSplitV2:shape:0?text_vectorization_5/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask22
0text_vectorization_5/StringSplit/strided_slice_1?
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_5/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2Y
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_5/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: 2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod?
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : 2g
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2?
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:?????????2f
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum?
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R 2d
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2LookupTableFindV2Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handle7text_vectorization_5/StringSplit/StringSplitV2:values:0Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:?????????2D
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
*text_vectorization_5/string_lookup_4/EqualEqual7text_vectorization_5/StringSplit/StringSplitV2:values:0,text_vectorization_5_string_lookup_4_equal_y*
T0*#
_output_shapes
:?????????2,
*text_vectorization_5/string_lookup_4/Equal?
-text_vectorization_5/string_lookup_4/SelectV2SelectV2.text_vectorization_5/string_lookup_4/Equal:z:0/text_vectorization_5_string_lookup_4_selectv2_tKtext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/SelectV2?
-text_vectorization_5/string_lookup_4/IdentityIdentity6text_vectorization_5/string_lookup_4/SelectV2:output:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/Identity?
$embedding_15/StatefulPartitionedCallStatefulPartitionedCall6text_vectorization_5/string_lookup_4/Identity:output:0btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0embedding_15_17229*
Tin
2		*
Tout
2	*
_collective_manager_ids
 *6
_output_shapes$
":?????????:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_15_layer_call_and_return_conditional_losses_167492&
$embedding_15/StatefulPartitionedCall?
+global_average_pooling1d_15/PartitionedCallPartitionedCall-embedding_15/StatefulPartitionedCall:output:0-embedding_15/StatefulPartitionedCall:output:1*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *_
fZRX
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_169222-
+global_average_pooling1d_15/PartitionedCall?
 dense_40/StatefulPartitionedCallStatefulPartitionedCall4global_average_pooling1d_15/PartitionedCall:output:0dense_40_17234dense_40_17236*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_40_layer_call_and_return_conditional_losses_169352"
 dense_40/StatefulPartitionedCall?
dropout_15/PartitionedCallPartitionedCall)dense_40/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_15_layer_call_and_return_conditional_losses_169462
dropout_15/PartitionedCall?
 dense_39/StatefulPartitionedCallStatefulPartitionedCall#dropout_15/PartitionedCall:output:0dense_39_17240dense_39_17242*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_39_layer_call_and_return_conditional_losses_169592"
 dense_39/StatefulPartitionedCall?
IdentityIdentity)dense_39/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity?
NoOpNoOp!^dense_39/StatefulPartitionedCall!^dense_40/StatefulPartitionedCall%^embedding_15/StatefulPartitionedCallC^text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2D
 dense_39/StatefulPartitionedCall dense_39/StatefulPartitionedCall2D
 dense_40/StatefulPartitionedCall dense_40/StatefulPartitionedCall2L
$embedding_15/StatefulPartitionedCall$embedding_15/StatefulPartitionedCall2?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_8:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
??
?
C__inference_model_17_layer_call_and_return_conditional_losses_17311
input_8S
Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handleT
Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value	0
,text_vectorization_5_string_lookup_4_equal_y3
/text_vectorization_5_string_lookup_4_selectv2_t	%
embedding_15_17294:	?N 
dense_40_17299:@
dense_40_17301:@ 
dense_39_17305:@
dense_39_17307:
identity?? dense_39/StatefulPartitionedCall? dense_40/StatefulPartitionedCall?"dropout_15/StatefulPartitionedCall?$embedding_15/StatefulPartitionedCall?Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
 text_vectorization_5/StringLowerStringLowerinput_8*'
_output_shapes
:?????????2"
 text_vectorization_5/StringLower?
'text_vectorization_5/StaticRegexReplaceStaticRegexReplace)text_vectorization_5/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite 2)
'text_vectorization_5/StaticRegexReplace?
text_vectorization_5/SqueezeSqueeze0text_vectorization_5/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????2
text_vectorization_5/Squeeze?
&text_vectorization_5/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B 2(
&text_vectorization_5/StringSplit/Const?
.text_vectorization_5/StringSplit/StringSplitV2StringSplitV2%text_vectorization_5/Squeeze:output:0/text_vectorization_5/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:20
.text_vectorization_5/StringSplit/StringSplitV2?
4text_vectorization_5/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        26
4text_vectorization_5/StringSplit/strided_slice/stack?
6text_vectorization_5/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       28
6text_vectorization_5/StringSplit/strided_slice/stack_1?
6text_vectorization_5/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      28
6text_vectorization_5/StringSplit/strided_slice/stack_2?
.text_vectorization_5/StringSplit/strided_sliceStridedSlice8text_vectorization_5/StringSplit/StringSplitV2:indices:0=text_vectorization_5/StringSplit/strided_slice/stack:output:0?text_vectorization_5/StringSplit/strided_slice/stack_1:output:0?text_vectorization_5/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask20
.text_vectorization_5/StringSplit/strided_slice?
6text_vectorization_5/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 28
6text_vectorization_5/StringSplit/strided_slice_1/stack?
8text_vectorization_5/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_1?
8text_vectorization_5/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_2?
0text_vectorization_5/StringSplit/strided_slice_1StridedSlice6text_vectorization_5/StringSplit/StringSplitV2:shape:0?text_vectorization_5/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask22
0text_vectorization_5/StringSplit/strided_slice_1?
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_5/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2Y
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_5/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: 2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod?
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : 2g
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2?
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:?????????2f
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum?
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R 2d
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2LookupTableFindV2Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handle7text_vectorization_5/StringSplit/StringSplitV2:values:0Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:?????????2D
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
*text_vectorization_5/string_lookup_4/EqualEqual7text_vectorization_5/StringSplit/StringSplitV2:values:0,text_vectorization_5_string_lookup_4_equal_y*
T0*#
_output_shapes
:?????????2,
*text_vectorization_5/string_lookup_4/Equal?
-text_vectorization_5/string_lookup_4/SelectV2SelectV2.text_vectorization_5/string_lookup_4/Equal:z:0/text_vectorization_5_string_lookup_4_selectv2_tKtext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/SelectV2?
-text_vectorization_5/string_lookup_4/IdentityIdentity6text_vectorization_5/string_lookup_4/SelectV2:output:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/Identity?
$embedding_15/StatefulPartitionedCallStatefulPartitionedCall6text_vectorization_5/string_lookup_4/Identity:output:0btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0embedding_15_17294*
Tin
2		*
Tout
2	*
_collective_manager_ids
 *6
_output_shapes$
":?????????:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_15_layer_call_and_return_conditional_losses_167492&
$embedding_15/StatefulPartitionedCall?
+global_average_pooling1d_15/PartitionedCallPartitionedCall-embedding_15/StatefulPartitionedCall:output:0-embedding_15/StatefulPartitionedCall:output:1*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *_
fZRX
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_169222-
+global_average_pooling1d_15/PartitionedCall?
 dense_40/StatefulPartitionedCallStatefulPartitionedCall4global_average_pooling1d_15/PartitionedCall:output:0dense_40_17299dense_40_17301*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_40_layer_call_and_return_conditional_losses_169352"
 dense_40/StatefulPartitionedCall?
"dropout_15/StatefulPartitionedCallStatefulPartitionedCall)dense_40/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_15_layer_call_and_return_conditional_losses_170172$
"dropout_15/StatefulPartitionedCall?
 dense_39/StatefulPartitionedCallStatefulPartitionedCall+dropout_15/StatefulPartitionedCall:output:0dense_39_17305dense_39_17307*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_39_layer_call_and_return_conditional_losses_169592"
 dense_39/StatefulPartitionedCall?
IdentityIdentity)dense_39/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity?
NoOpNoOp!^dense_39/StatefulPartitionedCall!^dense_40/StatefulPartitionedCall#^dropout_15/StatefulPartitionedCall%^embedding_15/StatefulPartitionedCallC^text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2D
 dense_39/StatefulPartitionedCall dense_39/StatefulPartitionedCall2D
 dense_40/StatefulPartitionedCall dense_40/StatefulPartitionedCall2H
"dropout_15/StatefulPartitionedCall"dropout_15/StatefulPartitionedCall2L
$embedding_15/StatefulPartitionedCall$embedding_15/StatefulPartitionedCall2?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_8:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
8
(__inference_restored_function_body_18168
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *%
f R
__inference__destroyer_150262
PartitionedCall[
IdentityIdentityPartitionedCall:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
?
C__inference_dense_40_layer_call_and_return_conditional_losses_18082

inputs0
matmul_readvariableop_resource:@-
biasadd_readvariableop_resource:@
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:@*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2	
BiasAddX
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
Relum
IdentityIdentityRelu:activations:0^NoOp*
T0*'
_output_shapes
:?????????@2

Identity
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?1
?
!__inference__traced_restore_18360
file_prefix;
(assignvariableop_embedding_15_embeddings:	?N4
"assignvariableop_1_dense_40_kernel:@.
 assignvariableop_2_dense_40_bias:@4
"assignvariableop_3_dense_39_kernel:@.
 assignvariableop_4_dense_39_bias:V
Lmutablehashtable_table_restore_lookuptableimportv2_statefulpartitionedcall_1: "
assignvariableop_5_total: "
assignvariableop_6_count: $
assignvariableop_7_total_1: $
assignvariableop_8_count_1: 
identity_10??AssignVariableOp?AssignVariableOp_1?AssignVariableOp_2?AssignVariableOp_3?AssignVariableOp_4?AssignVariableOp_5?AssignVariableOp_6?AssignVariableOp_7?AssignVariableOp_8?2MutableHashTable_table_restore/LookupTableImportV2?
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?
value?B?B:layer_with_weights-1/embeddings/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUEBLlayer_with_weights-0/_index_lookup_layer/token_counts/.ATTRIBUTES/table-keysBNlayer_with_weights-0/_index_lookup_layer/token_counts/.ATTRIBUTES/table-valuesB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
RestoreV2/tensor_names?
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*+
value"B B B B B B B B B B B B B 2
RestoreV2/shape_and_slices?
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*D
_output_shapes2
0::::::::::::*
dtypes
2	2
	RestoreV2g
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0*
_output_shapes
:2

Identity?
AssignVariableOpAssignVariableOp(assignvariableop_embedding_15_embeddingsIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOpk

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:2

Identity_1?
AssignVariableOp_1AssignVariableOp"assignvariableop_1_dense_40_kernelIdentity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_1k

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0*
_output_shapes
:2

Identity_2?
AssignVariableOp_2AssignVariableOp assignvariableop_2_dense_40_biasIdentity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_2k

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:2

Identity_3?
AssignVariableOp_3AssignVariableOp"assignvariableop_3_dense_39_kernelIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_3k

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:2

Identity_4?
AssignVariableOp_4AssignVariableOp assignvariableop_4_dense_39_biasIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_4?
2MutableHashTable_table_restore/LookupTableImportV2LookupTableImportV2Lmutablehashtable_table_restore_lookuptableimportv2_statefulpartitionedcall_1RestoreV2:tensors:5RestoreV2:tensors:6*	
Tin0*

Tout0	*,
_class"
 loc:@StatefulPartitionedCall_1*
_output_shapes
 24
2MutableHashTable_table_restore/LookupTableImportV2k

Identity_5IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:2

Identity_5?
AssignVariableOp_5AssignVariableOpassignvariableop_5_totalIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_5k

Identity_6IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:2

Identity_6?
AssignVariableOp_6AssignVariableOpassignvariableop_6_countIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_6k

Identity_7IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:2

Identity_7?
AssignVariableOp_7AssignVariableOpassignvariableop_7_total_1Identity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_7l

Identity_8IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:2

Identity_8?
AssignVariableOp_8AssignVariableOpassignvariableop_8_count_1Identity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_89
NoOpNoOp"/device:CPU:0*
_output_shapes
 2
NoOp?

Identity_9Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_2^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_83^MutableHashTable_table_restore/LookupTableImportV2^NoOp"/device:CPU:0*
T0*
_output_shapes
: 2

Identity_9e
Identity_10IdentityIdentity_9:output:0^NoOp_1*
T0*
_output_shapes
: 2
Identity_10?
NoOp_1NoOp^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_2^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_83^MutableHashTable_table_restore/LookupTableImportV2*"
_acd_function_control_output(*
_output_shapes
 2
NoOp_1"#
identity_10Identity_10:output:0*)
_input_shapes
: : : : : : : : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12(
AssignVariableOp_2AssignVariableOp_22(
AssignVariableOp_3AssignVariableOp_32(
AssignVariableOp_4AssignVariableOp_42(
AssignVariableOp_5AssignVariableOp_52(
AssignVariableOp_6AssignVariableOp_62(
AssignVariableOp_7AssignVariableOp_72(
AssignVariableOp_8AssignVariableOp_82h
2MutableHashTable_table_restore/LookupTableImportV22MutableHashTable_table_restore/LookupTableImportV2:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:2.
,
_class"
 loc:@StatefulPartitionedCall_1
?

?
(__inference_model_17_layer_call_fn_17359

inputs
unknown
	unknown_0	
	unknown_1
	unknown_2	
	unknown_3:	?N
	unknown_4:@
	unknown_5:@
	unknown_6:@
	unknown_7:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_model_17_layer_call_and_return_conditional_losses_169662
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?T
?
__inference_adapt_step_14310
iterator

iterator_19
5none_lookup_table_find_lookuptablefindv2_table_handle:
6none_lookup_table_find_lookuptablefindv2_default_value	??IteratorGetNext?(None_lookup_table_find/LookupTableFindV2?,None_lookup_table_insert/LookupTableInsertV2?
IteratorGetNextIteratorGetNextiterator*
_class
loc:@iterator*#
_output_shapes
:?????????*"
output_shapes
:?????????*
output_types
22
IteratorGetNextl
StringLowerStringLowerIteratorGetNext:components:0*#
_output_shapes
:?????????2
StringLower?
StaticRegexReplaceStaticRegexReplaceStringLower:output:0*#
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite 2
StaticRegexReplaceg
StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B 2
StringSplit/Const?
StringSplit/StringSplitV2StringSplitV2StaticRegexReplace:output:0StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:2
StringSplit/StringSplitV2?
StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2!
StringSplit/strided_slice/stack?
!StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       2#
!StringSplit/strided_slice/stack_1?
!StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2#
!StringSplit/strided_slice/stack_2?
StringSplit/strided_sliceStridedSlice#StringSplit/StringSplitV2:indices:0(StringSplit/strided_slice/stack:output:0*StringSplit/strided_slice/stack_1:output:0*StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask2
StringSplit/strided_slice?
!StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2#
!StringSplit/strided_slice_1/stack?
#StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2%
#StringSplit/strided_slice_1/stack_1?
#StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2%
#StringSplit/strided_slice_1/stack_2?
StringSplit/strided_slice_1StridedSlice!StringSplit/StringSplitV2:shape:0*StringSplit/strided_slice_1/stack:output:0,StringSplit/strided_slice_1/stack_1:output:0,StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2
StringSplit/strided_slice_1?
BStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast"StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2D
BStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast?
DStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast$StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: 2F
DStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1?
LStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShapeFStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:2N
LStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape?
LStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2N
LStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const?
KStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdUStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0UStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: 2M
KStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod?
PStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : 2R
PStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y?
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreaterTStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0YStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: 2P
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater?
KStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastRStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: 2M
KStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast?
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: 2P
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1?
JStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMaxFStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0WStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: 2L
JStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max?
LStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :2N
LStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y?
JStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2SStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0UStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: 2L
JStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add?
JStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMulOStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: 2L
JStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul?
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximumHStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: 2P
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum?
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimumHStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0RStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: 2P
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum?
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 2P
NStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2?
OStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincountFStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0RStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0WStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:?????????2Q
OStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount?
IStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : 2K
IStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis?
DStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumVStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0RStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:?????????2F
DStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum?
MStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R 2O
MStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0?
IStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2K
IStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis?
DStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2VStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0JStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0RStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:?????????2F
DStringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat?
UniqueWithCountsUniqueWithCounts"StringSplit/StringSplitV2:values:0*
T0*A
_output_shapes/
-:?????????:?????????:?????????*
out_idx0	2
UniqueWithCounts?
(None_lookup_table_find/LookupTableFindV2LookupTableFindV25none_lookup_table_find_lookuptablefindv2_table_handleUniqueWithCounts:y:06none_lookup_table_find_lookuptablefindv2_default_value",/job:localhost/replica:0/task:0/device:CPU:0*	
Tin0*

Tout0	*
_output_shapes
:2*
(None_lookup_table_find/LookupTableFindV2?
addAddV2UniqueWithCounts:count:01None_lookup_table_find/LookupTableFindV2:values:0*
T0	*
_output_shapes
:2
add?
,None_lookup_table_insert/LookupTableInsertV2LookupTableInsertV25none_lookup_table_find_lookuptablefindv2_table_handleUniqueWithCounts:y:0add:z:0)^None_lookup_table_find/LookupTableFindV2",/job:localhost/replica:0/task:0/device:CPU:0*	
Tin0*

Tout0	*
_output_shapes
 2.
,None_lookup_table_insert/LookupTableInsertV2*(
_construction_contextkEagerRuntime*
_input_shapes

: : : : 2"
IteratorGetNextIteratorGetNext2T
(None_lookup_table_find/LookupTableFindV2(None_lookup_table_find/LookupTableFindV22\
,None_lookup_table_insert/LookupTableInsertV2,None_lookup_table_insert/LookupTableInsertV2:( $
"
_user_specified_name
iterator:@<

_output_shapes
: 
"
_user_specified_name
iterator:

_output_shapes
: 
ɟ
?
C__inference_model_17_layer_call_and_return_conditional_losses_17615

inputsS
Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handleT
Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value	0
,text_vectorization_5_string_lookup_4_equal_y3
/text_vectorization_5_string_lookup_4_selectv2_t	N
;embedding_15_embedding_lookup_ragged_embedding_lookup_17430:	?N9
'dense_40_matmul_readvariableop_resource:@6
(dense_40_biasadd_readvariableop_resource:@9
'dense_39_matmul_readvariableop_resource:@6
(dense_39_biasadd_readvariableop_resource:
identity??dense_39/BiasAdd/ReadVariableOp?dense_39/MatMul/ReadVariableOp?dense_40/BiasAdd/ReadVariableOp?dense_40/MatMul/ReadVariableOp?5embedding_15/embedding_lookup_ragged/embedding_lookup?Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
 text_vectorization_5/StringLowerStringLowerinputs*'
_output_shapes
:?????????2"
 text_vectorization_5/StringLower?
'text_vectorization_5/StaticRegexReplaceStaticRegexReplace)text_vectorization_5/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite 2)
'text_vectorization_5/StaticRegexReplace?
text_vectorization_5/SqueezeSqueeze0text_vectorization_5/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????2
text_vectorization_5/Squeeze?
&text_vectorization_5/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B 2(
&text_vectorization_5/StringSplit/Const?
.text_vectorization_5/StringSplit/StringSplitV2StringSplitV2%text_vectorization_5/Squeeze:output:0/text_vectorization_5/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:20
.text_vectorization_5/StringSplit/StringSplitV2?
4text_vectorization_5/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        26
4text_vectorization_5/StringSplit/strided_slice/stack?
6text_vectorization_5/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       28
6text_vectorization_5/StringSplit/strided_slice/stack_1?
6text_vectorization_5/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      28
6text_vectorization_5/StringSplit/strided_slice/stack_2?
.text_vectorization_5/StringSplit/strided_sliceStridedSlice8text_vectorization_5/StringSplit/StringSplitV2:indices:0=text_vectorization_5/StringSplit/strided_slice/stack:output:0?text_vectorization_5/StringSplit/strided_slice/stack_1:output:0?text_vectorization_5/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask20
.text_vectorization_5/StringSplit/strided_slice?
6text_vectorization_5/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 28
6text_vectorization_5/StringSplit/strided_slice_1/stack?
8text_vectorization_5/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_1?
8text_vectorization_5/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2:
8text_vectorization_5/StringSplit/strided_slice_1/stack_2?
0text_vectorization_5/StringSplit/strided_slice_1StridedSlice6text_vectorization_5/StringSplit/StringSplitV2:shape:0?text_vectorization_5/StringSplit/strided_slice_1/stack:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_1:output:0Atext_vectorization_5/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask22
0text_vectorization_5/StringSplit/strided_slice_1?
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast7text_vectorization_5/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2Y
Wtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1Cast9text_vectorization_5/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: 2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShape[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdjtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod?
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : 2g
etext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreateritext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0ntext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater?
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastgtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: 2b
`text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMax[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max?
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :2c
atext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2htext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0jtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add?
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMuldtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: 2a
_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimum]text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum?
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 2e
ctext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2?
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincount[text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0ltext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:?????????2f
dtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum?
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R 2d
btext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0?
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2`
^text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis?
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2ktext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0_text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0gtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:?????????2[
Ytext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2LookupTableFindV2Otext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handle7text_vectorization_5/StringSplit/StringSplitV2:values:0Ptext_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:?????????2D
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
*text_vectorization_5/string_lookup_4/EqualEqual7text_vectorization_5/StringSplit/StringSplitV2:values:0,text_vectorization_5_string_lookup_4_equal_y*
T0*#
_output_shapes
:?????????2,
*text_vectorization_5/string_lookup_4/Equal?
-text_vectorization_5/string_lookup_4/SelectV2SelectV2.text_vectorization_5/string_lookup_4/Equal:z:0/text_vectorization_5_string_lookup_4_selectv2_tKtext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/SelectV2?
-text_vectorization_5/string_lookup_4/IdentityIdentity6text_vectorization_5/string_lookup_4/SelectV2:output:0*
T0	*#
_output_shapes
:?????????2/
-text_vectorization_5/string_lookup_4/Identity?
5embedding_15/embedding_lookup_ragged/embedding_lookupResourceGather;embedding_15_embedding_lookup_ragged_embedding_lookup_174306text_vectorization_5/string_lookup_4/Identity:output:0",/job:localhost/replica:0/task:0/device:CPU:0*
Tindices0	*N
_classD
B@loc:@embedding_15/embedding_lookup_ragged/embedding_lookup/17430*'
_output_shapes
:?????????*
dtype027
5embedding_15/embedding_lookup_ragged/embedding_lookup?
>embedding_15/embedding_lookup_ragged/embedding_lookup/IdentityIdentity>embedding_15/embedding_lookup_ragged/embedding_lookup:output:0",/job:localhost/replica:0/task:0/device:CPU:0*
T0*N
_classD
B@loc:@embedding_15/embedding_lookup_ragged/embedding_lookup/17430*'
_output_shapes
:?????????2@
>embedding_15/embedding_lookup_ragged/embedding_lookup/Identity?
@embedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1IdentityGembedding_15/embedding_lookup_ragged/embedding_lookup/Identity:output:0*
T0*'
_output_shapes
:?????????2B
@embedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1?
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/ShapeShapebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:2D
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/Shape?
Pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2R
Pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack?
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2T
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1?
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2T
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2?
Jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_sliceStridedSliceKglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/Shape:output:0Yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack:output:0[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1:output:0[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2L
Jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice?
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub/yConst*
_output_shapes
: *
dtype0*
value	B :2D
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub/y?
@global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/subSubSglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice:output:0Kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub/y:output:0*
T0*
_output_shapes
: 2B
@global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub?
iglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2k
iglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2?
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_sliceStridedSlicebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*
end_mask2e
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:
?????????2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2?
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1StridedSlicebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask2g
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1?
Yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/subSublglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice:output:0nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1:output:0*
T0	*#
_output_shapes
:?????????2[
Yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub?
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/ShapeShapebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:*
out_type0	2]
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Shape?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2?
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2StridedSlicedglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Shape:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2g
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2?
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/yConst*
_output_shapes
: *
dtype0	*
value	B	 R2_
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/y?
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1Subnglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2:output:0fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/y:output:0*
T0	*
_output_shapes
: 2]
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1?
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/startConst*
_output_shapes
: *
dtype0*
value	B : 2c
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/start?
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/deltaConst*
_output_shapes
: *
dtype0*
value	B :2c
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/delta?
`global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/CastCastjglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/start:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2b
`global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast?
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1Castjglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/delta:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2d
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1?
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/rangeRangedglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast:y:0_global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1:z:0fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1:y:0*

Tidx0	*#
_output_shapes
:?????????2]
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range?
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/CastCast]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub:z:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2c
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Cast?
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ShapeShapedglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range:output:0*
T0	*
_output_shapes
:2d
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Shape?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack?
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2t
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1?
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2t
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2?
jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_sliceStridedSlicekglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Shape:output:0yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack:output:0{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1:output:0{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2l
jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice?
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shapePacksglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice:output:0*
N*
T0*
_output_shapes
:2p
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape?
hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastToBroadcastToeglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Cast:y:0wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape:output:0*
T0*#
_output_shapes
:?????????2j
hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo?
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2d
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Const?
`global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/MaxMaxqglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Const:output:0*
T0*
_output_shapes
: 2b
`global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Max?
fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/xConst*
_output_shapes
: *
dtype0*
value	B : 2h
fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/x?
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/MaximumMaximumoglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/x:output:0iglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Max:output:0*
T0*
_output_shapes
: 2f
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ConstConst*
_output_shapes
: *
dtype0*
value	B : 2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1Const*
_output_shapes
: *
dtype0*
value	B :2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/RangeRangexglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const:output:0hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1:output:0*#
_output_shapes
:?????????2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range?
xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2z
xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim?
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims
ExpandDimsqglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2v
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims?
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/CastCast}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims:output:0*

DstT0*

SrcT0*'
_output_shapes
:?????????2p
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast?
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/LessLessxglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range:output:0rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast:y:0*
T0*0
_output_shapes
:??????????????????2p
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B :2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim?
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims
ExpandDimsdglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim:output:0*
T0	*'
_output_shapes
:?????????2i
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0Const*
_output_shapes
: *
dtype0*
value	B :2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiplesPackvglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0:output:0hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0*
N*
T0*
_output_shapes
:2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples?
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/TileTilepglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples:output:0*
T0	*0
_output_shapes
:??????????????????2c
aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ShapeShapejglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape?
}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2?
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_sliceStridedSlicexglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
:2y
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices?
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ProdProd?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices:output:0*
T0*
_output_shapes
: 2p
nglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1Shapejglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1StridedSlicezglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2Shapejglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2StridedSlicezglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1Packwglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod:output:0*
N*
T0*
_output_shapes
:2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1?
uglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2w
uglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concatConcatV2?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2:output:0~global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis:output:0*
N*
T0*
_output_shapes
:2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ReshapeReshapejglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat:output:0*
T0	*#
_output_shapes
:?????????2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*
valueB:
?????????2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1Reshaperglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less:z:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape:output:0*
T0
*#
_output_shapes
:?????????2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/WhereWhere|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1:output:0*'
_output_shapes
:?????????2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/SqueezeSqueezewglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where:index:0*
T0	*#
_output_shapes
:?????????*
squeeze_dims
2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze?
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axisConst*
_output_shapes
: *
dtype0*
value	B : 2y
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis?
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2GatherV2zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape:output:0zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis:output:0*
Taxis0*
Tindices0	*
Tparams0	*#
_output_shapes
:?????????2t
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2?
Oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSumUnsortedSegmentSumIembedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1:output:0{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2:output:0Dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub:z:0*
T0*
Tindices0	*'
_output_shapes
:?????????2Q
Oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSum?
<global_average_pooling1d_15/RaggedReduceMean/ones_like/ShapeShapeIembedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1:output:0*
T0*
_output_shapes
:2>
<global_average_pooling1d_15/RaggedReduceMean/ones_like/Shape?
<global_average_pooling1d_15/RaggedReduceMean/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2>
<global_average_pooling1d_15/RaggedReduceMean/ones_like/Const?
6global_average_pooling1d_15/RaggedReduceMean/ones_likeFillEglobal_average_pooling1d_15/RaggedReduceMean/ones_like/Shape:output:0Eglobal_average_pooling1d_15/RaggedReduceMean/ones_like/Const:output:0*
T0*'
_output_shapes
:?????????28
6global_average_pooling1d_15/RaggedReduceMean/ones_like?
Dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/ShapeShapebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:2F
Dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/Shape?
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2T
Rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack?
Tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2V
Tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1?
Tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2V
Tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2?
Lglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_sliceStridedSliceMglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/Shape:output:0[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack:output:0]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1:output:0]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2N
Lglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice?
Dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub/yConst*
_output_shapes
: *
dtype0*
value	B :2F
Dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub/y?
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/subSubUglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice:output:0Mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub/y:output:0*
T0*
_output_shapes
: 2D
Bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub?
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2m
kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2?
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_sliceStridedSlicebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*
end_mask2g
eglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:
?????????2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2?
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1StridedSlicebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack:output:0xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1:output:0xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask2i
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1?
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/subSubnglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice:output:0pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1:output:0*
T0	*#
_output_shapes
:?????????2]
[global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub?
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/ShapeShapebtext_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:*
out_type0	2_
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Shape?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2?
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2StridedSlicefglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Shape:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack:output:0xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1:output:0xglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2i
gglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2?
_global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/yConst*
_output_shapes
: *
dtype0	*
value	B	 R2a
_global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/y?
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1Subpglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2:output:0hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/y:output:0*
T0	*
_output_shapes
: 2_
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1?
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/startConst*
_output_shapes
: *
dtype0*
value	B : 2e
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/start?
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/deltaConst*
_output_shapes
: *
dtype0*
value	B :2e
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/delta?
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/CastCastlglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/start:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2d
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast?
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1Castlglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/delta:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2f
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1?
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/rangeRangefglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast:y:0aglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1:z:0hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1:y:0*

Tidx0	*#
_output_shapes
:?????????2_
]global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range?
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/CastCast_global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub:z:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2e
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Cast?
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ShapeShapefglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range:output:0*
T0	*
_output_shapes
:2f
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Shape?
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2t
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack?
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2v
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1?
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2v
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2?
lglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_sliceStridedSlicemglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Shape:output:0{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack:output:0}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1:output:0}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2n
lglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shapePackuglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice:output:0*
N*
T0*
_output_shapes
:2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape?
jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastToBroadcastTogglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Cast:y:0yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape:output:0*
T0*#
_output_shapes
:?????????2l
jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo?
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2f
dglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Const?
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/MaxMaxsglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Const:output:0*
T0*
_output_shapes
: 2d
bglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Max?
hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/xConst*
_output_shapes
: *
dtype0*
value	B : 2j
hglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/x?
fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/MaximumMaximumqglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/x:output:0kglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Max:output:0*
T0*
_output_shapes
: 2h
fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ConstConst*
_output_shapes
: *
dtype0*
value	B : 2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1Const*
_output_shapes
: *
dtype0*
value	B :2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/RangeRangezglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const:output:0jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1:output:0*#
_output_shapes
:?????????2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range?
zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2|
zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim?
vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims
ExpandDimssglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2x
vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/CastCastglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims:output:0*

DstT0*

SrcT0*'
_output_shapes
:?????????2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/LessLesszglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range:output:0tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast:y:0*
T0*0
_output_shapes
:??????????????????2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B :2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim?
iglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims
ExpandDimsfglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim:output:0*
T0	*'
_output_shapes
:?????????2k
iglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims?
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0Const*
_output_shapes
: *
dtype0*
value	B :2q
oglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0?
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiplesPackxglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0:output:0jglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0*
N*
T0*
_output_shapes
:2o
mglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples?
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/TileTilerglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims:output:0vglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples:output:0*
T0	*0
_output_shapes
:??????????????????2e
cglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ShapeShapelglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2?
global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_sliceStridedSlicezglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
:2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices?
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ProdProd?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices:output:0*
T0*
_output_shapes
: 2r
pglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1Shapelglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2?
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1StridedSlice|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2}
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2Shapelglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2?
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2StridedSlice|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2}
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2?
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1Packyglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod:output:0*
N*
T0*
_output_shapes
:2}
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1?
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2y
wglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis?
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concatConcatV2?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis:output:0*
N*
T0*
_output_shapes
:2t
rglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ReshapeReshapelglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat:output:0*
T0	*#
_output_shapes
:?????????2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape?
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*
valueB:
?????????2}
{global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape?
uglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1Reshapetglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less:z:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape:output:0*
T0
*#
_output_shapes
:?????????2w
uglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1?
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/WhereWhere~global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1:output:0*'
_output_shapes
:?????????2s
qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where?
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/SqueezeSqueezeyglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where:index:0*
T0	*#
_output_shapes
:?????????*
squeeze_dims
2u
sglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze?
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axisConst*
_output_shapes
: *
dtype0*
value	B : 2{
yglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis?
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2GatherV2|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape:output:0|global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze:output:0?global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis:output:0*
Taxis0*
Tindices0	*
Tparams0	*#
_output_shapes
:?????????2v
tglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2?
Qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSumUnsortedSegmentSum?global_average_pooling1d_15/RaggedReduceMean/ones_like:output:0}global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2:output:0Fglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub:z:0*
T0*
Tindices0	*'
_output_shapes
:?????????2S
Qglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSum?
4global_average_pooling1d_15/RaggedReduceMean/truedivRealDivXglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSum:output:0Zglobal_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSum:output:0*
T0*'
_output_shapes
:?????????26
4global_average_pooling1d_15/RaggedReduceMean/truediv?
dense_40/MatMul/ReadVariableOpReadVariableOp'dense_40_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02 
dense_40/MatMul/ReadVariableOp?
dense_40/MatMulMatMul8global_average_pooling1d_15/RaggedReduceMean/truediv:z:0&dense_40/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_40/MatMul?
dense_40/BiasAdd/ReadVariableOpReadVariableOp(dense_40_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype02!
dense_40/BiasAdd/ReadVariableOp?
dense_40/BiasAddBiasAdddense_40/MatMul:product:0'dense_40/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
dense_40/BiasAdds
dense_40/ReluReludense_40/BiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
dense_40/Relu?
dropout_15/IdentityIdentitydense_40/Relu:activations:0*
T0*'
_output_shapes
:?????????@2
dropout_15/Identity?
dense_39/MatMul/ReadVariableOpReadVariableOp'dense_39_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02 
dense_39/MatMul/ReadVariableOp?
dense_39/MatMulMatMuldropout_15/Identity:output:0&dense_39/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_39/MatMul?
dense_39/BiasAdd/ReadVariableOpReadVariableOp(dense_39_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02!
dense_39/BiasAdd/ReadVariableOp?
dense_39/BiasAddBiasAdddense_39/MatMul:product:0'dense_39/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_39/BiasAdd|
dense_39/SigmoidSigmoiddense_39/BiasAdd:output:0*
T0*'
_output_shapes
:?????????2
dense_39/Sigmoido
IdentityIdentitydense_39/Sigmoid:y:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity?
NoOpNoOp ^dense_39/BiasAdd/ReadVariableOp^dense_39/MatMul/ReadVariableOp ^dense_40/BiasAdd/ReadVariableOp^dense_40/MatMul/ReadVariableOp6^embedding_15/embedding_lookup_ragged/embedding_lookupC^text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2B
dense_39/BiasAdd/ReadVariableOpdense_39/BiasAdd/ReadVariableOp2@
dense_39/MatMul/ReadVariableOpdense_39/MatMul/ReadVariableOp2B
dense_40/BiasAdd/ReadVariableOpdense_40/BiasAdd/ReadVariableOp2@
dense_40/MatMul/ReadVariableOpdense_40/MatMul/ReadVariableOp2n
5embedding_15/embedding_lookup_ragged/embedding_lookup5embedding_15/embedding_lookup_ragged/embedding_lookup2?
Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2Btext_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
?
C__inference_dense_39_layer_call_and_return_conditional_losses_18129

inputs0
matmul_readvariableop_resource:@-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2	
BiasAdda
SigmoidSigmoidBiasAdd:output:0*
T0*'
_output_shapes
:?????????2	
Sigmoidf
IdentityIdentitySigmoid:y:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????@: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?
?
__inference__initializer_150679
5key_value_init136812_lookuptableimportv2_table_handle1
-key_value_init136812_lookuptableimportv2_keys3
/key_value_init136812_lookuptableimportv2_values	
identity??(key_value_init136812/LookupTableImportV2?
(key_value_init136812/LookupTableImportV2LookupTableImportV25key_value_init136812_lookuptableimportv2_table_handle-key_value_init136812_lookuptableimportv2_keys/key_value_init136812_lookuptableimportv2_values*	
Tin0*

Tout0	*
_output_shapes
 2*
(key_value_init136812/LookupTableImportV2P
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Consty
NoOpNoOp)^key_value_init136812/LookupTableImportV2*"
_acd_function_control_output(*
_output_shapes
 2
NoOpX
IdentityIdentityConst:output:0^NoOp*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*#
_input_shapes
: :?":?"2T
(key_value_init136812/LookupTableImportV2(key_value_init136812/LookupTableImportV2:!

_output_shapes	
:?":!

_output_shapes	
:?"
?
8
(__inference_restored_function_body_18188
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *'
f"R 
__inference__initializer_153982
PartitionedCall[
IdentityIdentityPartitionedCall:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
e
;__inference_global_average_pooling1d_15_layer_call_fn_17887

inputs
inputs_1	
identity?
PartitionedCallPartitionedCallinputsinputs_1*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *_
fZRX
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_169222
PartitionedCalll
IdentityIdentityPartitionedCall:output:0*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":?????????:?????????:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs
?

?
(__inference_model_17_layer_call_fn_16987
input_8
unknown
	unknown_0	
	unknown_1
	unknown_2	
	unknown_3:	?N
	unknown_4:@
	unknown_5:@
	unknown_6:@
	unknown_7:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinput_8unknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7*
Tin
2
		*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*'
_read_only_resource_inputs	
	*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_model_17_layer_call_and_return_conditional_losses_169662
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_8:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
c
E__inference_dropout_15_layer_call_and_return_conditional_losses_16946

inputs

identity_1Z
IdentityIdentityinputs*
T0*'
_output_shapes
:?????????@2

Identityi

Identity_1IdentityIdentity:output:0*
T0*'
_output_shapes
:?????????@2

Identity_1"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????@:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?
F
*__inference_dropout_15_layer_call_fn_18087

inputs
identity?
PartitionedCallPartitionedCallinputs*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@* 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *N
fIRG
E__inference_dropout_15_layer_call_and_return_conditional_losses_169462
PartitionedCalll
IdentityIdentityPartitionedCall:output:0*
T0*'
_output_shapes
:?????????@2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*&
_input_shapes
:?????????@:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?
?
(__inference_dense_40_layer_call_fn_18071

inputs
unknown:@
	unknown_0:@
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????@*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *L
fGRE
C__inference_dense_40_layer_call_and_return_conditional_losses_169352
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????@2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?	
?
,__inference_embedding_15_layer_call_fn_17865

inputs	
inputs_1	
unknown:	?N
identity

identity_1	??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsinputs_1unknown*
Tin
2		*
Tout
2	*
_collective_manager_ids
 *6
_output_shapes$
":?????????:?????????*#
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_embedding_15_layer_call_and_return_conditional_losses_167492
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity{

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0	*#
_output_shapes
:?????????2

Identity_1h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0*(
_construction_contextkEagerRuntime*3
_input_shapes"
 :?????????:?????????: 22
StatefulPartitionedCallStatefulPartitionedCall:K G
#
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs
?$
?
__inference__traced_save_18320
file_prefix6
2savev2_embedding_15_embeddings_read_readvariableop.
*savev2_dense_40_kernel_read_readvariableop,
(savev2_dense_40_bias_read_readvariableop.
*savev2_dense_39_kernel_read_readvariableop,
(savev2_dense_39_bias_read_readvariableop>
:savev2_none_lookup_table_export_values_lookuptableexportv2@
<savev2_none_lookup_table_export_values_lookuptableexportv2_1	$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop&
"savev2_total_1_read_readvariableop&
"savev2_count_1_read_readvariableop
savev2_const_6

identity_1??MergeV2Checkpoints?
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*2
StaticRegexFullMatchc
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.part2
Constl
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B
_temp/part2	
Const_1?
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: 2
Selectt

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: 2

StringJoinZ

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :2

num_shards
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : 2
ShardedFilename/shard?
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: 2
ShardedFilename?
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?
value?B?B:layer_with_weights-1/embeddings/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUEBLlayer_with_weights-0/_index_lookup_layer/token_counts/.ATTRIBUTES/table-keysBNlayer_with_weights-0/_index_lookup_layer/token_counts/.ATTRIBUTES/table-valuesB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
SaveV2/tensor_names?
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*+
value"B B B B B B B B B B B B B 2
SaveV2/shape_and_slices?
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:02savev2_embedding_15_embeddings_read_readvariableop*savev2_dense_40_kernel_read_readvariableop(savev2_dense_40_bias_read_readvariableop*savev2_dense_39_kernel_read_readvariableop(savev2_dense_39_bias_read_readvariableop:savev2_none_lookup_table_export_values_lookuptableexportv2<savev2_none_lookup_table_export_values_lookuptableexportv2_1 savev2_total_read_readvariableop savev2_count_read_readvariableop"savev2_total_1_read_readvariableop"savev2_count_1_read_readvariableopsavev2_const_6"/device:CPU:0*
_output_shapes
 *
dtypes
2	2
SaveV2?
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0^SaveV2"/device:CPU:0*
N*
T0*
_output_shapes
:2(
&MergeV2Checkpoints/checkpoint_prefixes?
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix"/device:CPU:0*
_output_shapes
 2
MergeV2Checkpointsr
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: 2

Identity_

Identity_1IdentityIdentity:output:0^NoOp*
T0*
_output_shapes
: 2

Identity_1c
NoOpNoOp^MergeV2Checkpoints*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"!

identity_1Identity_1:output:0*R
_input_shapesA
?: :	?N:@:@:@:::: : : : : 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:%!

_output_shapes
:	?N:$ 

_output_shapes

:@: 

_output_shapes
:@:$ 

_output_shapes

:@: 

_output_shapes
::

_output_shapes
::

_output_shapes
::

_output_shapes
: :	

_output_shapes
: :


_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?
?
C__inference_dense_39_layer_call_and_return_conditional_losses_16959

inputs0
matmul_readvariableop_resource:@-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2	
BiasAdda
SigmoidSigmoidBiasAdd:output:0*
T0*'
_output_shapes
:?????????2	
Sigmoidf
IdentityIdentitySigmoid:y:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????@: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????@
 
_user_specified_nameinputs
?
[
(__inference_restored_function_body_18268
identity: ??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall*	
Tin
 *
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *#
fR
__inference__creator_145682
StatefulPartitionedCallj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*
_input_shapes 22
StatefulPartitionedCallStatefulPartitionedCall
ݤ
?
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_16922

inputs
inputs_1	
identity?
&RaggedReduceMean/RaggedReduceSum/ShapeShapeinputs_1*
T0	*
_output_shapes
:2(
&RaggedReduceMean/RaggedReduceSum/Shape?
4RaggedReduceMean/RaggedReduceSum/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 26
4RaggedReduceMean/RaggedReduceSum/strided_slice/stack?
6RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:28
6RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1?
6RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:28
6RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2?
.RaggedReduceMean/RaggedReduceSum/strided_sliceStridedSlice/RaggedReduceMean/RaggedReduceSum/Shape:output:0=RaggedReduceMean/RaggedReduceSum/strided_slice/stack:output:0?RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1:output:0?RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask20
.RaggedReduceMean/RaggedReduceSum/strided_slice?
&RaggedReduceMean/RaggedReduceSum/sub/yConst*
_output_shapes
: *
dtype0*
value	B :2(
&RaggedReduceMean/RaggedReduceSum/sub/y?
$RaggedReduceMean/RaggedReduceSum/subSub7RaggedReduceMean/RaggedReduceSum/strided_slice:output:0/RaggedReduceMean/RaggedReduceSum/sub/y:output:0*
T0*
_output_shapes
: 2&
$RaggedReduceMean/RaggedReduceSum/sub?
MRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2O
MRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2?
GRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_sliceStridedSliceinputs_1VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack:output:0XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1:output:0XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*
end_mask2I
GRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack?
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:
?????????2S
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1?
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2S
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2?
IRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1StridedSliceinputs_1XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack:output:0ZRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1:output:0ZRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask2K
IRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1?
=RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/subSubPRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice:output:0RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1:output:0*
T0	*#
_output_shapes
:?????????2?
=RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub?
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/ShapeShapeinputs_1*
T0	*
_output_shapes
:*
out_type0	2A
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Shape?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack?
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2S
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1?
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2S
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2?
IRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2StridedSliceHRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Shape:output:0XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack:output:0ZRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1:output:0ZRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2K
IRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2?
ARaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/yConst*
_output_shapes
: *
dtype0	*
value	B	 R2C
ARaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/y?
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1SubRRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2:output:0JRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/y:output:0*
T0	*
_output_shapes
: 2A
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1?
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/startConst*
_output_shapes
: *
dtype0*
value	B : 2G
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/start?
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/deltaConst*
_output_shapes
: *
dtype0*
value	B :2G
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/delta?
DRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/CastCastNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/start:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2F
DRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast?
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1CastNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/delta:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2H
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1?
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/rangeRangeHRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast:y:0CRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1:z:0JRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1:y:0*

Tidx0	*#
_output_shapes
:?????????2A
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range?
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/CastCastARaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub:z:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2G
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Cast?
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ShapeShapeHRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range:output:0*
T0	*
_output_shapes
:2H
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Shape?
TRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2V
TRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack?
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2X
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1?
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2X
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2?
NRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_sliceStridedSliceORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Shape:output:0]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack:output:0_RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1:output:0_RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2P
NRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice?
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shapePackWRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice:output:0*
N*
T0*
_output_shapes
:2T
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape?
LRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastToBroadcastToIRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Cast:y:0[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape:output:0*
T0*#
_output_shapes
:?????????2N
LRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo?
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2H
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Const?
DRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/MaxMaxURaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Const:output:0*
T0*
_output_shapes
: 2F
DRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Max?
JRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/xConst*
_output_shapes
: *
dtype0*
value	B : 2L
JRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/x?
HRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/MaximumMaximumSRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/x:output:0MRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Max:output:0*
T0*
_output_shapes
: 2J
HRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum?
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ConstConst*
_output_shapes
: *
dtype0*
value	B : 2U
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const?
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1Const*
_output_shapes
: *
dtype0*
value	B :2W
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1?
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/RangeRange\RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const:output:0LRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0^RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1:output:0*#
_output_shapes
:?????????2U
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range?
\RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2^
\RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim?
XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims
ExpandDimsURaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2Z
XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims?
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/CastCastaRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims:output:0*

DstT0*

SrcT0*'
_output_shapes
:?????????2T
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast?
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/LessLess\RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range:output:0VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast:y:0*
T0*0
_output_shapes
:??????????????????2T
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B :2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim?
KRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims
ExpandDimsHRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range:output:0XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim:output:0*
T0	*'
_output_shapes
:?????????2M
KRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims?
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0Const*
_output_shapes
: *
dtype0*
value	B :2S
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiplesPackZRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0:output:0LRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0*
N*
T0*
_output_shapes
:2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples?
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/TileTileTRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims:output:0XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples:output:0*
T0	*0
_output_shapes
:??????????????????2G
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile?
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ShapeShapeNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2U
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape?
aRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2c
aRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack?
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2e
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1?
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2e
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2?
[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_sliceStridedSlice\RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape:output:0jRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack:output:0lRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1:output:0lRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
:2]
[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice?
dRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: 2f
dRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices?
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ProdProddRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice:output:0mRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices:output:0*
T0*
_output_shapes
: 2T
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod?
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1ShapeNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2W
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1?
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2e
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack?
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2g
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1?
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2g
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2?
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1StridedSlice^RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1:output:0lRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack:output:0nRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1:output:0nRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2_
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1?
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2ShapeNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2W
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2?
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:2e
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack?
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2g
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1?
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2g
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2?
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2StridedSlice^RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2:output:0lRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack:output:0nRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1:output:0nRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2_
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2?
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1Pack[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod:output:0*
N*
T0*
_output_shapes
:2_
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1?
YRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2[
YRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis?
TRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concatConcatV2fRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1:output:0fRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1:output:0fRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2:output:0bRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis:output:0*
N*
T0*
_output_shapes
:2V
TRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat?
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ReshapeReshapeNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat:output:0*
T0	*#
_output_shapes
:?????????2W
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape?
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*
valueB:
?????????2_
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape?
WRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1ReshapeVRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less:z:0fRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape:output:0*
T0
*#
_output_shapes
:?????????2Y
WRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1?
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/WhereWhere`RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1:output:0*'
_output_shapes
:?????????2U
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where?
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/SqueezeSqueeze[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where:index:0*
T0	*#
_output_shapes
:?????????*
squeeze_dims
2W
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze?
[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axisConst*
_output_shapes
: *
dtype0*
value	B : 2]
[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis?
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2GatherV2^RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape:output:0^RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze:output:0dRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis:output:0*
Taxis0*
Tindices0	*
Tparams0	*#
_output_shapes
:?????????2X
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2?
3RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSumUnsortedSegmentSuminputs_RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2:output:0(RaggedReduceMean/RaggedReduceSum/sub:z:0*
T0*
Tindices0	*'
_output_shapes
:?????????25
3RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSumz
 RaggedReduceMean/ones_like/ShapeShapeinputs*
T0*
_output_shapes
:2"
 RaggedReduceMean/ones_like/Shape?
 RaggedReduceMean/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2"
 RaggedReduceMean/ones_like/Const?
RaggedReduceMean/ones_likeFill)RaggedReduceMean/ones_like/Shape:output:0)RaggedReduceMean/ones_like/Const:output:0*
T0*'
_output_shapes
:?????????2
RaggedReduceMean/ones_like?
(RaggedReduceMean/RaggedReduceSum_1/ShapeShapeinputs_1*
T0	*
_output_shapes
:2*
(RaggedReduceMean/RaggedReduceSum_1/Shape?
6RaggedReduceMean/RaggedReduceSum_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 28
6RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack?
8RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2:
8RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1?
8RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2:
8RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2?
0RaggedReduceMean/RaggedReduceSum_1/strided_sliceStridedSlice1RaggedReduceMean/RaggedReduceSum_1/Shape:output:0?RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack:output:0ARaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1:output:0ARaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask22
0RaggedReduceMean/RaggedReduceSum_1/strided_slice?
(RaggedReduceMean/RaggedReduceSum_1/sub/yConst*
_output_shapes
: *
dtype0*
value	B :2*
(RaggedReduceMean/RaggedReduceSum_1/sub/y?
&RaggedReduceMean/RaggedReduceSum_1/subSub9RaggedReduceMean/RaggedReduceSum_1/strided_slice:output:01RaggedReduceMean/RaggedReduceSum_1/sub/y:output:0*
T0*
_output_shapes
: 2(
&RaggedReduceMean/RaggedReduceSum_1/sub?
ORaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2Q
ORaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2?
IRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_sliceStridedSliceinputs_1XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack:output:0ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1:output:0ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*
end_mask2K
IRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack?
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:
?????????2U
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1?
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2U
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2?
KRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1StridedSliceinputs_1ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack:output:0\RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1:output:0\RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask2M
KRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1?
?RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/subSubRRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice:output:0TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1:output:0*
T0	*#
_output_shapes
:?????????2A
?RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub?
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/ShapeShapeinputs_1*
T0	*
_output_shapes
:*
out_type0	2C
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Shape?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack?
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2U
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1?
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2U
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2?
KRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2StridedSliceJRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Shape:output:0ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack:output:0\RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1:output:0\RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2M
KRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2?
CRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/yConst*
_output_shapes
: *
dtype0	*
value	B	 R2E
CRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/y?
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1SubTRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2:output:0LRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/y:output:0*
T0	*
_output_shapes
: 2C
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1?
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/startConst*
_output_shapes
: *
dtype0*
value	B : 2I
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/start?
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/deltaConst*
_output_shapes
: *
dtype0*
value	B :2I
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/delta?
FRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/CastCastPRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/start:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2H
FRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast?
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1CastPRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/delta:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2J
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1?
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/rangeRangeJRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast:y:0ERaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1:z:0LRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1:y:0*

Tidx0	*#
_output_shapes
:?????????2C
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range?
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/CastCastCRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub:z:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2I
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Cast?
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ShapeShapeJRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range:output:0*
T0	*
_output_shapes
:2J
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Shape?
VRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2X
VRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack?
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2Z
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1?
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2Z
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2?
PRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_sliceStridedSliceQRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Shape:output:0_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack:output:0aRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1:output:0aRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2R
PRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice?
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shapePackYRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice:output:0*
N*
T0*
_output_shapes
:2V
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape?
NRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastToBroadcastToKRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Cast:y:0]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape:output:0*
T0*#
_output_shapes
:?????????2P
NRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo?
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2J
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Const?
FRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/MaxMaxWRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Const:output:0*
T0*
_output_shapes
: 2H
FRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Max?
LRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/xConst*
_output_shapes
: *
dtype0*
value	B : 2N
LRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/x?
JRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/MaximumMaximumURaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/x:output:0ORaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Max:output:0*
T0*
_output_shapes
: 2L
JRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum?
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ConstConst*
_output_shapes
: *
dtype0*
value	B : 2W
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const?
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1Const*
_output_shapes
: *
dtype0*
value	B :2Y
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1?
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/RangeRange^RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const:output:0NRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0`RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1:output:0*#
_output_shapes
:?????????2W
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range?
^RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2`
^RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim?
ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims
ExpandDimsWRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2\
ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims?
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/CastCastcRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims:output:0*

DstT0*

SrcT0*'
_output_shapes
:?????????2V
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast?
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/LessLess^RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range:output:0XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast:y:0*
T0*0
_output_shapes
:??????????????????2V
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B :2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim?
MRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims
ExpandDimsJRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range:output:0ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim:output:0*
T0	*'
_output_shapes
:?????????2O
MRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims?
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0Const*
_output_shapes
: *
dtype0*
value	B :2U
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiplesPack\RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0:output:0NRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0*
N*
T0*
_output_shapes
:2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples?
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/TileTileVRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims:output:0ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples:output:0*
T0	*0
_output_shapes
:??????????????????2I
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile?
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ShapeShapePRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2W
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape?
cRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2e
cRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack?
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2g
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1?
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2g
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2?
]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_sliceStridedSlice^RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape:output:0lRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack:output:0nRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1:output:0nRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
:2_
]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice?
fRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: 2h
fRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices?
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ProdProdfRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice:output:0oRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices:output:0*
T0*
_output_shapes
: 2V
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod?
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1ShapePRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2Y
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1?
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2g
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack?
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2i
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1?
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2i
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2?
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1StridedSlice`RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1:output:0nRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack:output:0pRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1:output:0pRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2a
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1?
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2ShapePRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2Y
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2?
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:2g
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack?
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2i
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1?
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2i
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2?
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2StridedSlice`RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2:output:0nRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack:output:0pRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1:output:0pRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2a
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2?
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1Pack]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod:output:0*
N*
T0*
_output_shapes
:2a
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1?
[RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2]
[RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis?
VRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concatConcatV2hRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1:output:0hRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1:output:0hRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2:output:0dRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis:output:0*
N*
T0*
_output_shapes
:2X
VRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat?
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ReshapeReshapePRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat:output:0*
T0	*#
_output_shapes
:?????????2Y
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape?
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*
valueB:
?????????2a
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape?
YRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1ReshapeXRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less:z:0hRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape:output:0*
T0
*#
_output_shapes
:?????????2[
YRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1?
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/WhereWherebRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1:output:0*'
_output_shapes
:?????????2W
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where?
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/SqueezeSqueeze]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where:index:0*
T0	*#
_output_shapes
:?????????*
squeeze_dims
2Y
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze?
]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axisConst*
_output_shapes
: *
dtype0*
value	B : 2_
]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis?
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2GatherV2`RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape:output:0`RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze:output:0fRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis:output:0*
Taxis0*
Tindices0	*
Tparams0	*#
_output_shapes
:?????????2Z
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2?
5RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSumUnsortedSegmentSum#RaggedReduceMean/ones_like:output:0aRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2:output:0*RaggedReduceMean/RaggedReduceSum_1/sub:z:0*
T0*
Tindices0	*'
_output_shapes
:?????????27
5RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSum?
RaggedReduceMean/truedivRealDiv<RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSum:output:0>RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSum:output:0*
T0*'
_output_shapes
:?????????2
RaggedReduceMean/truedivp
IdentityIdentityRaggedReduceMean/truediv:z:0*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":?????????:?????????:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
(__inference_restored_function_body_15074
unknown
	unknown_0
	unknown_1	
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallunknown	unknown_0	unknown_1*
Tin
2	*
Tout
2*
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *'
f"R 
__inference__initializer_150672
StatefulPartitionedCallh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOpj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*#
_input_shapes
: :?":?"22
StatefulPartitionedCallStatefulPartitionedCall:!

_output_shapes	
:?":!

_output_shapes	
:?"
?
M
__inference__creator_14568
identity: ??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_145642
StatefulPartitionedCallh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOpj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 22
StatefulPartitionedCallStatefulPartitionedCall
?
G
__inference__creator_15056
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_150522
StatefulPartitionedCallh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOpj
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 22
StatefulPartitionedCallStatefulPartitionedCall
ݤ
?
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_18062

inputs
inputs_1	
identity?
&RaggedReduceMean/RaggedReduceSum/ShapeShapeinputs_1*
T0	*
_output_shapes
:2(
&RaggedReduceMean/RaggedReduceSum/Shape?
4RaggedReduceMean/RaggedReduceSum/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 26
4RaggedReduceMean/RaggedReduceSum/strided_slice/stack?
6RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:28
6RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1?
6RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:28
6RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2?
.RaggedReduceMean/RaggedReduceSum/strided_sliceStridedSlice/RaggedReduceMean/RaggedReduceSum/Shape:output:0=RaggedReduceMean/RaggedReduceSum/strided_slice/stack:output:0?RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1:output:0?RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask20
.RaggedReduceMean/RaggedReduceSum/strided_slice?
&RaggedReduceMean/RaggedReduceSum/sub/yConst*
_output_shapes
: *
dtype0*
value	B :2(
&RaggedReduceMean/RaggedReduceSum/sub/y?
$RaggedReduceMean/RaggedReduceSum/subSub7RaggedReduceMean/RaggedReduceSum/strided_slice:output:0/RaggedReduceMean/RaggedReduceSum/sub/y:output:0*
T0*
_output_shapes
: 2&
$RaggedReduceMean/RaggedReduceSum/sub?
MRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2O
MRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2?
GRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_sliceStridedSliceinputs_1VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack:output:0XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1:output:0XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*
end_mask2I
GRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack?
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:
?????????2S
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1?
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2S
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2?
IRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1StridedSliceinputs_1XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack:output:0ZRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1:output:0ZRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask2K
IRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1?
=RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/subSubPRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice:output:0RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1:output:0*
T0	*#
_output_shapes
:?????????2?
=RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub?
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/ShapeShapeinputs_1*
T0	*
_output_shapes
:*
out_type0	2A
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Shape?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack?
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2S
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1?
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2S
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2?
IRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2StridedSliceHRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Shape:output:0XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack:output:0ZRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1:output:0ZRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2K
IRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2?
ARaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/yConst*
_output_shapes
: *
dtype0	*
value	B	 R2C
ARaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/y?
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1SubRRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2:output:0JRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/y:output:0*
T0	*
_output_shapes
: 2A
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1?
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/startConst*
_output_shapes
: *
dtype0*
value	B : 2G
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/start?
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/deltaConst*
_output_shapes
: *
dtype0*
value	B :2G
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/delta?
DRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/CastCastNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/start:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2F
DRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast?
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1CastNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/delta:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2H
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1?
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/rangeRangeHRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast:y:0CRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1:z:0JRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1:y:0*

Tidx0	*#
_output_shapes
:?????????2A
?RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range?
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/CastCastARaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub:z:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2G
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Cast?
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ShapeShapeHRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range:output:0*
T0	*
_output_shapes
:2H
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Shape?
TRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2V
TRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack?
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2X
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1?
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2X
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2?
NRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_sliceStridedSliceORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Shape:output:0]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack:output:0_RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1:output:0_RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2P
NRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice?
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shapePackWRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice:output:0*
N*
T0*
_output_shapes
:2T
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape?
LRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastToBroadcastToIRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Cast:y:0[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape:output:0*
T0*#
_output_shapes
:?????????2N
LRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo?
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2H
FRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Const?
DRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/MaxMaxURaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Const:output:0*
T0*
_output_shapes
: 2F
DRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Max?
JRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/xConst*
_output_shapes
: *
dtype0*
value	B : 2L
JRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/x?
HRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/MaximumMaximumSRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/x:output:0MRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Max:output:0*
T0*
_output_shapes
: 2J
HRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum?
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ConstConst*
_output_shapes
: *
dtype0*
value	B : 2U
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const?
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1Const*
_output_shapes
: *
dtype0*
value	B :2W
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1?
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/RangeRange\RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const:output:0LRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0^RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1:output:0*#
_output_shapes
:?????????2U
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range?
\RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2^
\RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim?
XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims
ExpandDimsURaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2Z
XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims?
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/CastCastaRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims:output:0*

DstT0*

SrcT0*'
_output_shapes
:?????????2T
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast?
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/LessLess\RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range:output:0VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast:y:0*
T0*0
_output_shapes
:??????????????????2T
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B :2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim?
KRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims
ExpandDimsHRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range:output:0XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim:output:0*
T0	*'
_output_shapes
:?????????2M
KRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims?
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0Const*
_output_shapes
: *
dtype0*
value	B :2S
QRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0?
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiplesPackZRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0:output:0LRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0*
N*
T0*
_output_shapes
:2Q
ORaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples?
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/TileTileTRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims:output:0XRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples:output:0*
T0	*0
_output_shapes
:??????????????????2G
ERaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile?
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ShapeShapeNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2U
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape?
aRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2c
aRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack?
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2e
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1?
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2e
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2?
[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_sliceStridedSlice\RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape:output:0jRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack:output:0lRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1:output:0lRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
:2]
[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice?
dRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: 2f
dRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices?
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ProdProddRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice:output:0mRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices:output:0*
T0*
_output_shapes
: 2T
RRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod?
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1ShapeNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2W
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1?
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2e
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack?
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2g
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1?
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2g
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2?
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1StridedSlice^RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1:output:0lRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack:output:0nRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1:output:0nRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2_
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1?
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2ShapeNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2W
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2?
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:2e
cRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack?
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2g
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1?
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2g
eRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2?
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2StridedSlice^RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2:output:0lRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack:output:0nRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1:output:0nRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2_
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2?
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1Pack[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod:output:0*
N*
T0*
_output_shapes
:2_
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1?
YRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2[
YRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis?
TRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concatConcatV2fRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1:output:0fRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1:output:0fRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2:output:0bRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis:output:0*
N*
T0*
_output_shapes
:2V
TRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat?
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ReshapeReshapeNRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat:output:0*
T0	*#
_output_shapes
:?????????2W
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape?
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*
valueB:
?????????2_
]RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape?
WRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1ReshapeVRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less:z:0fRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape:output:0*
T0
*#
_output_shapes
:?????????2Y
WRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1?
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/WhereWhere`RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1:output:0*'
_output_shapes
:?????????2U
SRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where?
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/SqueezeSqueeze[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where:index:0*
T0	*#
_output_shapes
:?????????*
squeeze_dims
2W
URaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze?
[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axisConst*
_output_shapes
: *
dtype0*
value	B : 2]
[RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis?
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2GatherV2^RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape:output:0^RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze:output:0dRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis:output:0*
Taxis0*
Tindices0	*
Tparams0	*#
_output_shapes
:?????????2X
VRaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2?
3RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSumUnsortedSegmentSuminputs_RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2:output:0(RaggedReduceMean/RaggedReduceSum/sub:z:0*
T0*
Tindices0	*'
_output_shapes
:?????????25
3RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSumz
 RaggedReduceMean/ones_like/ShapeShapeinputs*
T0*
_output_shapes
:2"
 RaggedReduceMean/ones_like/Shape?
 RaggedReduceMean/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2"
 RaggedReduceMean/ones_like/Const?
RaggedReduceMean/ones_likeFill)RaggedReduceMean/ones_like/Shape:output:0)RaggedReduceMean/ones_like/Const:output:0*
T0*'
_output_shapes
:?????????2
RaggedReduceMean/ones_like?
(RaggedReduceMean/RaggedReduceSum_1/ShapeShapeinputs_1*
T0	*
_output_shapes
:2*
(RaggedReduceMean/RaggedReduceSum_1/Shape?
6RaggedReduceMean/RaggedReduceSum_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 28
6RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack?
8RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2:
8RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1?
8RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2:
8RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2?
0RaggedReduceMean/RaggedReduceSum_1/strided_sliceStridedSlice1RaggedReduceMean/RaggedReduceSum_1/Shape:output:0?RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack:output:0ARaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1:output:0ARaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask22
0RaggedReduceMean/RaggedReduceSum_1/strided_slice?
(RaggedReduceMean/RaggedReduceSum_1/sub/yConst*
_output_shapes
: *
dtype0*
value	B :2*
(RaggedReduceMean/RaggedReduceSum_1/sub/y?
&RaggedReduceMean/RaggedReduceSum_1/subSub9RaggedReduceMean/RaggedReduceSum_1/strided_slice:output:01RaggedReduceMean/RaggedReduceSum_1/sub/y:output:0*
T0*
_output_shapes
: 2(
&RaggedReduceMean/RaggedReduceSum_1/sub?
ORaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2Q
ORaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2?
IRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_sliceStridedSliceinputs_1XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack:output:0ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1:output:0ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*
end_mask2K
IRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack?
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:
?????????2U
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1?
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2U
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2?
KRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1StridedSliceinputs_1ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack:output:0\RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1:output:0\RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask2M
KRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1?
?RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/subSubRRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice:output:0TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1:output:0*
T0	*#
_output_shapes
:?????????2A
?RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub?
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/ShapeShapeinputs_1*
T0	*
_output_shapes
:*
out_type0	2C
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Shape?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack?
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2U
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1?
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2U
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2?
KRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2StridedSliceJRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Shape:output:0ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack:output:0\RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1:output:0\RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2M
KRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2?
CRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/yConst*
_output_shapes
: *
dtype0	*
value	B	 R2E
CRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/y?
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1SubTRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2:output:0LRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/y:output:0*
T0	*
_output_shapes
: 2C
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1?
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/startConst*
_output_shapes
: *
dtype0*
value	B : 2I
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/start?
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/deltaConst*
_output_shapes
: *
dtype0*
value	B :2I
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/delta?
FRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/CastCastPRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/start:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2H
FRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast?
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1CastPRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/delta:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2J
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1?
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/rangeRangeJRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast:y:0ERaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1:z:0LRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1:y:0*

Tidx0	*#
_output_shapes
:?????????2C
ARaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range?
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/CastCastCRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub:z:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2I
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Cast?
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ShapeShapeJRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range:output:0*
T0	*
_output_shapes
:2J
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Shape?
VRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2X
VRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack?
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2Z
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1?
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2Z
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2?
PRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_sliceStridedSliceQRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Shape:output:0_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack:output:0aRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1:output:0aRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2R
PRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice?
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shapePackYRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice:output:0*
N*
T0*
_output_shapes
:2V
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape?
NRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastToBroadcastToKRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Cast:y:0]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape:output:0*
T0*#
_output_shapes
:?????????2P
NRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo?
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2J
HRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Const?
FRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/MaxMaxWRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Const:output:0*
T0*
_output_shapes
: 2H
FRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Max?
LRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/xConst*
_output_shapes
: *
dtype0*
value	B : 2N
LRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/x?
JRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/MaximumMaximumURaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/x:output:0ORaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Max:output:0*
T0*
_output_shapes
: 2L
JRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum?
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ConstConst*
_output_shapes
: *
dtype0*
value	B : 2W
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const?
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1Const*
_output_shapes
: *
dtype0*
value	B :2Y
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1?
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/RangeRange^RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const:output:0NRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0`RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1:output:0*#
_output_shapes
:?????????2W
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range?
^RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2`
^RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim?
ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims
ExpandDimsWRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2\
ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims?
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/CastCastcRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims:output:0*

DstT0*

SrcT0*'
_output_shapes
:?????????2V
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast?
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/LessLess^RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range:output:0XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast:y:0*
T0*0
_output_shapes
:??????????????????2V
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B :2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim?
MRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims
ExpandDimsJRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range:output:0ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim:output:0*
T0	*'
_output_shapes
:?????????2O
MRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims?
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0Const*
_output_shapes
: *
dtype0*
value	B :2U
SRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0?
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiplesPack\RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0:output:0NRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0*
N*
T0*
_output_shapes
:2S
QRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples?
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/TileTileVRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims:output:0ZRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples:output:0*
T0	*0
_output_shapes
:??????????????????2I
GRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile?
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ShapeShapePRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2W
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape?
cRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2e
cRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack?
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2g
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1?
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2g
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2?
]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_sliceStridedSlice^RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape:output:0lRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack:output:0nRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1:output:0nRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
:2_
]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice?
fRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: 2h
fRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices?
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ProdProdfRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice:output:0oRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices:output:0*
T0*
_output_shapes
: 2V
TRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod?
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1ShapePRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2Y
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1?
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2g
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack?
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2i
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1?
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2i
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2?
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1StridedSlice`RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1:output:0nRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack:output:0pRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1:output:0pRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2a
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1?
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2ShapePRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2Y
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2?
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:2g
eRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack?
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2i
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1?
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2i
gRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2?
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2StridedSlice`RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2:output:0nRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack:output:0pRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1:output:0pRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2a
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2?
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1Pack]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod:output:0*
N*
T0*
_output_shapes
:2a
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1?
[RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2]
[RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis?
VRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concatConcatV2hRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1:output:0hRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1:output:0hRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2:output:0dRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis:output:0*
N*
T0*
_output_shapes
:2X
VRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat?
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ReshapeReshapePRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat:output:0*
T0	*#
_output_shapes
:?????????2Y
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape?
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*
valueB:
?????????2a
_RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape?
YRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1ReshapeXRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less:z:0hRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape:output:0*
T0
*#
_output_shapes
:?????????2[
YRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1?
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/WhereWherebRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1:output:0*'
_output_shapes
:?????????2W
URaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where?
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/SqueezeSqueeze]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where:index:0*
T0	*#
_output_shapes
:?????????*
squeeze_dims
2Y
WRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze?
]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axisConst*
_output_shapes
: *
dtype0*
value	B : 2_
]RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis?
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2GatherV2`RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape:output:0`RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze:output:0fRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis:output:0*
Taxis0*
Tindices0	*
Tparams0	*#
_output_shapes
:?????????2Z
XRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2?
5RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSumUnsortedSegmentSum#RaggedReduceMean/ones_like:output:0aRaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2:output:0*RaggedReduceMean/RaggedReduceSum_1/sub:z:0*
T0*
Tindices0	*'
_output_shapes
:?????????27
5RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSum?
RaggedReduceMean/truedivRealDiv<RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSum:output:0>RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSum:output:0*
T0*'
_output_shapes
:?????????2
RaggedReduceMean/truedivp
IdentityIdentityRaggedReduceMean/truediv:z:0*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*5
_input_shapes$
":?????????:?????????:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:KG
#
_output_shapes
:?????????
 
_user_specified_nameinputs
?
,
__inference__destroyer_15017
identityP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
ConstQ
IdentityIdentityConst:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
,
__inference__destroyer_15026
identity?
PartitionedCallPartitionedCall*	
Tin
 *
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *1
f,R*
(__inference_restored_function_body_150212
PartitionedCallP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
ConstQ
IdentityIdentityConst:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
?
?
C__inference_dense_40_layer_call_and_return_conditional_losses_16935

inputs0
matmul_readvariableop_resource:@-
biasadd_readvariableop_resource:@
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:@*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2	
BiasAddX
ReluReluBiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
Relum
IdentityIdentityRelu:activations:0^NoOp*
T0*'
_output_shapes
:?????????@2

Identity
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
.
__inference__initializer_15389
identityP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
ConstQ
IdentityIdentityConst:output:0*
T0*
_output_shapes
: 2

Identity"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*
_input_shapes 
??
?
 __inference__wrapped_model_16662
input_8\
Xmodel_17_text_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handle]
Ymodel_17_text_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value	9
5model_17_text_vectorization_5_string_lookup_4_equal_y<
8model_17_text_vectorization_5_string_lookup_4_selectv2_t	W
Dmodel_17_embedding_15_embedding_lookup_ragged_embedding_lookup_16477:	?NB
0model_17_dense_40_matmul_readvariableop_resource:@?
1model_17_dense_40_biasadd_readvariableop_resource:@B
0model_17_dense_39_matmul_readvariableop_resource:@?
1model_17_dense_39_biasadd_readvariableop_resource:
identity??(model_17/dense_39/BiasAdd/ReadVariableOp?'model_17/dense_39/MatMul/ReadVariableOp?(model_17/dense_40/BiasAdd/ReadVariableOp?'model_17/dense_40/MatMul/ReadVariableOp?>model_17/embedding_15/embedding_lookup_ragged/embedding_lookup?Kmodel_17/text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
)model_17/text_vectorization_5/StringLowerStringLowerinput_8*'
_output_shapes
:?????????2+
)model_17/text_vectorization_5/StringLower?
0model_17/text_vectorization_5/StaticRegexReplaceStaticRegexReplace2model_17/text_vectorization_5/StringLower:output:0*'
_output_shapes
:?????????*6
pattern+)[!"#$%&()\*\+,-\./:;<=>?@\[\\\]^_`{|}~\']*
rewrite 22
0model_17/text_vectorization_5/StaticRegexReplace?
%model_17/text_vectorization_5/SqueezeSqueeze9model_17/text_vectorization_5/StaticRegexReplace:output:0*
T0*#
_output_shapes
:?????????*
squeeze_dims

?????????2'
%model_17/text_vectorization_5/Squeeze?
/model_17/text_vectorization_5/StringSplit/ConstConst*
_output_shapes
: *
dtype0*
valueB B 21
/model_17/text_vectorization_5/StringSplit/Const?
7model_17/text_vectorization_5/StringSplit/StringSplitV2StringSplitV2.model_17/text_vectorization_5/Squeeze:output:08model_17/text_vectorization_5/StringSplit/Const:output:0*<
_output_shapes*
(:?????????:?????????:29
7model_17/text_vectorization_5/StringSplit/StringSplitV2?
=model_17/text_vectorization_5/StringSplit/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB"        2?
=model_17/text_vectorization_5/StringSplit/strided_slice/stack?
?model_17/text_vectorization_5/StringSplit/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB"       2A
?model_17/text_vectorization_5/StringSplit/strided_slice/stack_1?
?model_17/text_vectorization_5/StringSplit/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB"      2A
?model_17/text_vectorization_5/StringSplit/strided_slice/stack_2?
7model_17/text_vectorization_5/StringSplit/strided_sliceStridedSliceAmodel_17/text_vectorization_5/StringSplit/StringSplitV2:indices:0Fmodel_17/text_vectorization_5/StringSplit/strided_slice/stack:output:0Hmodel_17/text_vectorization_5/StringSplit/strided_slice/stack_1:output:0Hmodel_17/text_vectorization_5/StringSplit/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask*
end_mask*
shrink_axis_mask29
7model_17/text_vectorization_5/StringSplit/strided_slice?
?model_17/text_vectorization_5/StringSplit/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2A
?model_17/text_vectorization_5/StringSplit/strided_slice_1/stack?
Amodel_17/text_vectorization_5/StringSplit/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2C
Amodel_17/text_vectorization_5/StringSplit/strided_slice_1/stack_1?
Amodel_17/text_vectorization_5/StringSplit/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2C
Amodel_17/text_vectorization_5/StringSplit/strided_slice_1/stack_2?
9model_17/text_vectorization_5/StringSplit/strided_slice_1StridedSlice?model_17/text_vectorization_5/StringSplit/StringSplitV2:shape:0Hmodel_17/text_vectorization_5/StringSplit/strided_slice_1/stack:output:0Jmodel_17/text_vectorization_5/StringSplit/strided_slice_1/stack_1:output:0Jmodel_17/text_vectorization_5/StringSplit/strided_slice_1/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2;
9model_17/text_vectorization_5/StringSplit/strided_slice_1?
`model_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CastCast@model_17/text_vectorization_5/StringSplit/strided_slice:output:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2b
`model_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast?
bmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1CastBmodel_17/text_vectorization_5/StringSplit/strided_slice_1:output:0*

DstT0*

SrcT0	*
_output_shapes
: 2d
bmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1?
jmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ShapeShapedmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0*
T0*
_output_shapes
:2l
jmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape?
jmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2l
jmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const?
imodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/ProdProdsmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Shape:output:0smodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const:output:0*
T0*
_output_shapes
: 2k
imodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod?
nmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/yConst*
_output_shapes
: *
dtype0*
value	B : 2p
nmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y?
lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/GreaterGreaterrmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Prod:output:0wmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater/y:output:0*
T0*
_output_shapes
: 2n
lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater?
imodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/CastCastpmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Greater:z:0*

DstT0*

SrcT0
*
_output_shapes
: 2k
imodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast?
lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1Const*
_output_shapes
:*
dtype0*
valueB: 2n
lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1?
hmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaxMaxdmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0umodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_1:output:0*
T0*
_output_shapes
: 2j
hmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max?
jmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/yConst*
_output_shapes
: *
dtype0*
value	B :2l
jmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y?
hmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/addAddV2qmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Max:output:0smodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add/y:output:0*
T0*
_output_shapes
: 2j
hmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add?
hmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mulMulmmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Cast:y:0lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/add:z:0*
T0*
_output_shapes
: 2j
hmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul?
lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MaximumMaximumfmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/mul:z:0*
T0*
_output_shapes
: 2n
lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum?
lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/MinimumMinimumfmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast_1:y:0pmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Maximum:z:0*
T0*
_output_shapes
: 2n
lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum?
lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2Const*
_output_shapes
: *
dtype0	*
valueB	 2n
lmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2?
mmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/BincountBincountdmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cast:y:0pmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Minimum:z:0umodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Const_2:output:0*
T0	*#
_output_shapes
:?????????2o
mmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount?
gmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axisConst*
_output_shapes
: *
dtype0*
value	B : 2i
gmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis?
bmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/CumsumCumsumtmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/bincount/Bincount:bins:0pmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum/axis:output:0*
T0	*#
_output_shapes
:?????????2d
bmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum?
kmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0Const*
_output_shapes
:*
dtype0	*
valueB	R 2m
kmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0?
gmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2i
gmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis?
bmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concatConcatV2tmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/values_0:output:0hmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/Cumsum:out:0pmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat/axis:output:0*
N*
T0	*#
_output_shapes
:?????????2d
bmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat?
Kmodel_17/text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2LookupTableFindV2Xmodel_17_text_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_table_handle@model_17/text_vectorization_5/StringSplit/StringSplitV2:values:0Ymodel_17_text_vectorization_5_string_lookup_4_none_lookup_lookuptablefindv2_default_value*	
Tin0*

Tout0	*#
_output_shapes
:?????????2M
Kmodel_17/text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2?
3model_17/text_vectorization_5/string_lookup_4/EqualEqual@model_17/text_vectorization_5/StringSplit/StringSplitV2:values:05model_17_text_vectorization_5_string_lookup_4_equal_y*
T0*#
_output_shapes
:?????????25
3model_17/text_vectorization_5/string_lookup_4/Equal?
6model_17/text_vectorization_5/string_lookup_4/SelectV2SelectV27model_17/text_vectorization_5/string_lookup_4/Equal:z:08model_17_text_vectorization_5_string_lookup_4_selectv2_tTmodel_17/text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:values:0*
T0	*#
_output_shapes
:?????????28
6model_17/text_vectorization_5/string_lookup_4/SelectV2?
6model_17/text_vectorization_5/string_lookup_4/IdentityIdentity?model_17/text_vectorization_5/string_lookup_4/SelectV2:output:0*
T0	*#
_output_shapes
:?????????28
6model_17/text_vectorization_5/string_lookup_4/Identity?
>model_17/embedding_15/embedding_lookup_ragged/embedding_lookupResourceGatherDmodel_17_embedding_15_embedding_lookup_ragged_embedding_lookup_16477?model_17/text_vectorization_5/string_lookup_4/Identity:output:0",/job:localhost/replica:0/task:0/device:CPU:0*
Tindices0	*W
_classM
KIloc:@model_17/embedding_15/embedding_lookup_ragged/embedding_lookup/16477*'
_output_shapes
:?????????*
dtype02@
>model_17/embedding_15/embedding_lookup_ragged/embedding_lookup?
Gmodel_17/embedding_15/embedding_lookup_ragged/embedding_lookup/IdentityIdentityGmodel_17/embedding_15/embedding_lookup_ragged/embedding_lookup:output:0",/job:localhost/replica:0/task:0/device:CPU:0*
T0*W
_classM
KIloc:@model_17/embedding_15/embedding_lookup_ragged/embedding_lookup/16477*'
_output_shapes
:?????????2I
Gmodel_17/embedding_15/embedding_lookup_ragged/embedding_lookup/Identity?
Imodel_17/embedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1IdentityPmodel_17/embedding_15/embedding_lookup_ragged/embedding_lookup/Identity:output:0*
T0*'
_output_shapes
:?????????2K
Imodel_17/embedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1?
Kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/ShapeShapekmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:2M
Kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/Shape?
Ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2[
Ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack?
[model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2]
[model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1?
[model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2]
[model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2?
Smodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_sliceStridedSliceTmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/Shape:output:0bmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack:output:0dmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_1:output:0dmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2U
Smodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice?
Kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub/yConst*
_output_shapes
: *
dtype0*
value	B :2M
Kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub/y?
Imodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/subSub\model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/strided_slice:output:0Tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub/y:output:0*
T0*
_output_shapes
: 2K
Imodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub?
rmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2t
rmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack?
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2v
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1?
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2v
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2?
lmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_sliceStridedSlicekmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack:output:0}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_1:output:0}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*
end_mask2n
lmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice?
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2v
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack?
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:
?????????2x
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1?
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2x
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2?
nmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1StridedSlicekmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack:output:0model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_1:output:0model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask2p
nmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1?
bmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/subSubumodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice:output:0wmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_1:output:0*
T0	*#
_output_shapes
:?????????2d
bmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub?
dmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/ShapeShapekmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:*
out_type0	2f
dmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Shape?
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2v
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack?
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2x
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1?
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2x
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2?
nmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2StridedSlicemmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Shape:output:0}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack:output:0model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_1:output:0model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2p
nmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2?
fmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/yConst*
_output_shapes
: *
dtype0	*
value	B	 R2h
fmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/y?
dmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1Subwmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/strided_slice_2:output:0omodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1/y:output:0*
T0	*
_output_shapes
: 2f
dmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1?
jmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/startConst*
_output_shapes
: *
dtype0*
value	B : 2l
jmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/start?
jmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/deltaConst*
_output_shapes
: *
dtype0*
value	B :2l
jmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/delta?
imodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/CastCastsmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/start:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2k
imodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast?
kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1Castsmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/delta:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2m
kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1?
dmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/rangeRangemmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast:y:0hmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub_1:z:0omodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range/Cast_1:y:0*

Tidx0	*#
_output_shapes
:?????????2f
dmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range?
jmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/CastCastfmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/sub:z:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2l
jmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Cast?
kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ShapeShapemmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range:output:0*
T0	*
_output_shapes
:2m
kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Shape?
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2{
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack?
{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2}
{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1?
{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2}
{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2?
smodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_sliceStridedSlicetmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Shape:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2u
smodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice?
wmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shapePack|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/strided_slice:output:0*
N*
T0*
_output_shapes
:2y
wmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape?
qmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastToBroadcastTonmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Cast:y:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape:output:0*
T0*#
_output_shapes
:?????????2s
qmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo?
kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2m
kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Const?
imodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/MaxMaxzmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Const:output:0*
T0*
_output_shapes
: 2k
imodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Max?
omodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/xConst*
_output_shapes
: *
dtype0*
value	B : 2q
omodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/x?
mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/MaximumMaximumxmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum/x:output:0rmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Max:output:0*
T0*
_output_shapes
: 2o
mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum?
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ConstConst*
_output_shapes
: *
dtype0*
value	B : 2z
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const?
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1Const*
_output_shapes
: *
dtype0*
value	B :2|
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1?
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/RangeRange?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const:output:0qmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1:output:0*#
_output_shapes
:?????????2z
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim?
}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims
ExpandDimszmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2
}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims?
wmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/CastCast?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims:output:0*

DstT0*

SrcT0*'
_output_shapes
:?????????2y
wmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast?
wmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/LessLess?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range:output:0{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast:y:0*
T0*0
_output_shapes
:??????????????????2y
wmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less?
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B :2v
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim?
pmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims
ExpandDimsmmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/range:output:0}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim:output:0*
T0	*'
_output_shapes
:?????????2r
pmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims?
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0Const*
_output_shapes
: *
dtype0*
value	B :2x
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0?
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiplesPackmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0:output:0qmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0*
N*
T0*
_output_shapes
:2v
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples?
jmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/TileTileymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/ExpandDims:output:0}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile/multiples:output:0*
T0	*0
_output_shapes
:??????????????????2l
jmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile?
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ShapeShapesmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2z
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_sliceStridedSlice?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices?
wmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ProdProd?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices:output:0*
T0*
_output_shapes
: 2y
wmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod?
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1Shapesmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2|
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1StridedSlice?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1?
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2Shapesmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2|
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2StridedSlice?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1Pack?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod:output:0*
N*
T0*
_output_shapes
:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1?
~model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2?
~model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis?
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concatConcatV2?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis:output:0*
N*
T0*
_output_shapes
:2{
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat?
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ReshapeReshapesmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/Tile:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat:output:0*
T0	*#
_output_shapes
:?????????2|
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*
valueB:
?????????2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape?
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1Reshape{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less:z:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape:output:0*
T0
*#
_output_shapes
:?????????2~
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1?
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/WhereWhere?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1:output:0*'
_output_shapes
:?????????2z
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where?
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/SqueezeSqueeze?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where:index:0*
T0	*#
_output_shapes
:?????????*
squeeze_dims
2|
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axisConst*
_output_shapes
: *
dtype0*
value	B : 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis?
{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2GatherV2?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis:output:0*
Taxis0*
Tindices0	*
Tparams0	*#
_output_shapes
:?????????2}
{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2?
Xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSumUnsortedSegmentSumRmodel_17/embedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2:output:0Mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/sub:z:0*
T0*
Tindices0	*'
_output_shapes
:?????????2Z
Xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSum?
Emodel_17/global_average_pooling1d_15/RaggedReduceMean/ones_like/ShapeShapeRmodel_17/embedding_15/embedding_lookup_ragged/embedding_lookup/Identity_1:output:0*
T0*
_output_shapes
:2G
Emodel_17/global_average_pooling1d_15/RaggedReduceMean/ones_like/Shape?
Emodel_17/global_average_pooling1d_15/RaggedReduceMean/ones_like/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2G
Emodel_17/global_average_pooling1d_15/RaggedReduceMean/ones_like/Const?
?model_17/global_average_pooling1d_15/RaggedReduceMean/ones_likeFillNmodel_17/global_average_pooling1d_15/RaggedReduceMean/ones_like/Shape:output:0Nmodel_17/global_average_pooling1d_15/RaggedReduceMean/ones_like/Const:output:0*
T0*'
_output_shapes
:?????????2A
?model_17/global_average_pooling1d_15/RaggedReduceMean/ones_like?
Mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/ShapeShapekmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:2O
Mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/Shape?
[model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2]
[model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack?
]model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2_
]model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1?
]model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2_
]model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2?
Umodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_sliceStridedSliceVmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/Shape:output:0dmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack:output:0fmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_1:output:0fmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2W
Umodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice?
Mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub/yConst*
_output_shapes
: *
dtype0*
value	B :2O
Mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub/y?
Kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/subSub^model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/strided_slice:output:0Vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub/y:output:0*
T0*
_output_shapes
: 2M
Kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub?
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2v
tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack?
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2x
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1?
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2x
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2?
nmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_sliceStridedSlicekmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack:output:0model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_1:output:0model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*
end_mask2p
nmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice?
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2x
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack?
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:
?????????2z
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1?
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2z
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2?
pmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1StridedSlicekmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1/stack_2:output:0*
Index0*
T0	*#
_output_shapes
:?????????*

begin_mask2r
pmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1?
dmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/subSubwmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice:output:0ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_1:output:0*
T0	*#
_output_shapes
:?????????2f
dmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub?
fmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/ShapeShapekmodel_17/text_vectorization_5/StringSplit/RaggedFromValueRowIds/RowPartitionFromValueRowIds/concat:output:0*
T0	*
_output_shapes
:*
out_type0	2h
fmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Shape?
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2x
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack?
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2z
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1?
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2z
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2?
pmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2StridedSliceomodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Shape:output:0model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2/stack_2:output:0*
Index0*
T0	*
_output_shapes
: *
shrink_axis_mask2r
pmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2?
hmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/yConst*
_output_shapes
: *
dtype0	*
value	B	 R2j
hmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/y?
fmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1Subymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/strided_slice_2:output:0qmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1/y:output:0*
T0	*
_output_shapes
: 2h
fmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1?
lmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/startConst*
_output_shapes
: *
dtype0*
value	B : 2n
lmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/start?
lmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/deltaConst*
_output_shapes
: *
dtype0*
value	B :2n
lmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/delta?
kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/CastCastumodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/start:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2m
kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast?
mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1Castumodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/delta:output:0*

DstT0	*

SrcT0*
_output_shapes
: 2o
mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1?
fmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/rangeRangeomodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast:y:0jmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub_1:z:0qmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range/Cast_1:y:0*

Tidx0	*#
_output_shapes
:?????????2h
fmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range?
lmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/CastCasthmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/sub:z:0*

DstT0*

SrcT0	*#
_output_shapes
:?????????2n
lmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Cast?
mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ShapeShapeomodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range:output:0*
T0	*
_output_shapes
:2o
mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Shape?
{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2}
{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack?
}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1?
}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2?
umodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_sliceStridedSlicevmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Shape:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2w
umodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice?
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shapePack~model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/strided_slice:output:0*
N*
T0*
_output_shapes
:2{
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape?
smodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastToBroadcastTopmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Cast:y:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo/shape:output:0*
T0*#
_output_shapes
:?????????2u
smodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo?
mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ConstConst*
_output_shapes
:*
dtype0*
valueB: 2o
mmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Const?
kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/MaxMax|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Const:output:0*
T0*
_output_shapes
: 2m
kmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Max?
qmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/xConst*
_output_shapes
: *
dtype0*
value	B : 2s
qmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/x?
omodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/MaximumMaximumzmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum/x:output:0tmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Max:output:0*
T0*
_output_shapes
: 2q
omodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum?
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ConstConst*
_output_shapes
: *
dtype0*
value	B : 2|
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const?
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1Const*
_output_shapes
: *
dtype0*
value	B :2~
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1?
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/RangeRange?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const:output:0smodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Const_1:output:0*#
_output_shapes
:?????????2|
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
valueB :
?????????2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim?
model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims
ExpandDims|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/BroadcastTo:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims/dim:output:0*
T0*'
_output_shapes
:?????????2?
model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims?
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/CastCast?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/ExpandDims:output:0*

DstT0*

SrcT0*'
_output_shapes
:?????????2{
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast?
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/LessLess?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Range:output:0}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Cast:y:0*
T0*0
_output_shapes
:??????????????????2{
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less?
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dimConst*
_output_shapes
: *
dtype0*
value	B :2x
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim?
rmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims
ExpandDimsomodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/range:output:0model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims/dim:output:0*
T0	*'
_output_shapes
:?????????2t
rmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims?
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0Const*
_output_shapes
: *
dtype0*
value	B :2z
xmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0?
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiplesPack?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples/0:output:0smodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Maximum:z:0*
N*
T0*
_output_shapes
:2x
vmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples?
lmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/TileTile{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/ExpandDims:output:0model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile/multiples:output:0*
T0	*0
_output_shapes
:??????????????????2n
lmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile?
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ShapeShapeumodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2|
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_sliceStridedSlice?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indicesConst*
_output_shapes
:*
dtype0*
valueB: 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices?
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ProdProd?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod/reduction_indices:output:0*
T0*
_output_shapes
: 2{
ymodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod?
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1Shapeumodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2~
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1StridedSlice?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *

begin_mask2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1?
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2Shapeumodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0*
T0	*
_output_shapes
:2~
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2StridedSlice?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Shape_2:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
end_mask2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1Pack?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Prod:output:0*
N*
T0*
_output_shapes
:2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axisConst*
_output_shapes
: *
dtype0*
value	B : 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis?
{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concatConcatV2?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/values_1:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/strided_slice_2:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat/axis:output:0*
N*
T0*
_output_shapes
:2}
{model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat?
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/ReshapeReshapeumodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/Tile:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/concat:output:0*
T0	*#
_output_shapes
:?????????2~
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*
valueB:
?????????2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape?
~model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1Reshape}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/SequenceMask/Less:z:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1/shape:output:0*
T0
*#
_output_shapes
:?????????2?
~model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1?
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/WhereWhere?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape_1:output:0*'
_output_shapes
:?????????2|
zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where?
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/SqueezeSqueeze?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Where:index:0*
T0	*#
_output_shapes
:?????????*
squeeze_dims
2~
|model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axisConst*
_output_shapes
: *
dtype0*
value	B : 2?
?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis?
}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2GatherV2?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Reshape:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/Squeeze:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2/axis:output:0*
Taxis0*
Tindices0	*
Tparams0	*#
_output_shapes
:?????????2
}model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2?
Zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSumUnsortedSegmentSumHmodel_17/global_average_pooling1d_15/RaggedReduceMean/ones_like:output:0?model_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/RaggedSplitsToSegmentIds/Repeat/boolean_mask/GatherV2:output:0Omodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/sub:z:0*
T0*
Tindices0	*'
_output_shapes
:?????????2\
Zmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSum?
=model_17/global_average_pooling1d_15/RaggedReduceMean/truedivRealDivamodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum/UnsortedSegmentSum:output:0cmodel_17/global_average_pooling1d_15/RaggedReduceMean/RaggedReduceSum_1/UnsortedSegmentSum:output:0*
T0*'
_output_shapes
:?????????2?
=model_17/global_average_pooling1d_15/RaggedReduceMean/truediv?
'model_17/dense_40/MatMul/ReadVariableOpReadVariableOp0model_17_dense_40_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02)
'model_17/dense_40/MatMul/ReadVariableOp?
model_17/dense_40/MatMulMatMulAmodel_17/global_average_pooling1d_15/RaggedReduceMean/truediv:z:0/model_17/dense_40/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
model_17/dense_40/MatMul?
(model_17/dense_40/BiasAdd/ReadVariableOpReadVariableOp1model_17_dense_40_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype02*
(model_17/dense_40/BiasAdd/ReadVariableOp?
model_17/dense_40/BiasAddBiasAdd"model_17/dense_40/MatMul:product:00model_17/dense_40/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????@2
model_17/dense_40/BiasAdd?
model_17/dense_40/ReluRelu"model_17/dense_40/BiasAdd:output:0*
T0*'
_output_shapes
:?????????@2
model_17/dense_40/Relu?
model_17/dropout_15/IdentityIdentity$model_17/dense_40/Relu:activations:0*
T0*'
_output_shapes
:?????????@2
model_17/dropout_15/Identity?
'model_17/dense_39/MatMul/ReadVariableOpReadVariableOp0model_17_dense_39_matmul_readvariableop_resource*
_output_shapes

:@*
dtype02)
'model_17/dense_39/MatMul/ReadVariableOp?
model_17/dense_39/MatMulMatMul%model_17/dropout_15/Identity:output:0/model_17/dense_39/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
model_17/dense_39/MatMul?
(model_17/dense_39/BiasAdd/ReadVariableOpReadVariableOp1model_17_dense_39_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02*
(model_17/dense_39/BiasAdd/ReadVariableOp?
model_17/dense_39/BiasAddBiasAdd"model_17/dense_39/MatMul:product:00model_17/dense_39/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
model_17/dense_39/BiasAdd?
model_17/dense_39/SigmoidSigmoid"model_17/dense_39/BiasAdd:output:0*
T0*'
_output_shapes
:?????????2
model_17/dense_39/Sigmoidx
IdentityIdentitymodel_17/dense_39/Sigmoid:y:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity?
NoOpNoOp)^model_17/dense_39/BiasAdd/ReadVariableOp(^model_17/dense_39/MatMul/ReadVariableOp)^model_17/dense_40/BiasAdd/ReadVariableOp(^model_17/dense_40/MatMul/ReadVariableOp?^model_17/embedding_15/embedding_lookup_ragged/embedding_lookupL^model_17/text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*8
_input_shapes'
%:?????????: : : : : : : : : 2T
(model_17/dense_39/BiasAdd/ReadVariableOp(model_17/dense_39/BiasAdd/ReadVariableOp2R
'model_17/dense_39/MatMul/ReadVariableOp'model_17/dense_39/MatMul/ReadVariableOp2T
(model_17/dense_40/BiasAdd/ReadVariableOp(model_17/dense_40/BiasAdd/ReadVariableOp2R
'model_17/dense_40/MatMul/ReadVariableOp'model_17/dense_40/MatMul/ReadVariableOp2?
>model_17/embedding_15/embedding_lookup_ragged/embedding_lookup>model_17/embedding_15/embedding_lookup_ragged/embedding_lookup2?
Kmodel_17/text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2Kmodel_17/text_vectorization_5/string_lookup_4/None_Lookup/LookupTableFindV2:P L
'
_output_shapes
:?????????
!
_user_specified_name	input_8:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: "?L
saver_filename:0StatefulPartitionedCall_4:0StatefulPartitionedCall_58"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*?
serving_default?
;
input_80
serving_default_input_8:0?????????>
dense_392
StatefulPartitionedCall_3:0?????????tensorflow/serving/predict:??
?
layer-0
layer_with_weights-0
layer-1
layer_with_weights-1
layer-2
layer-3
layer_with_weights-2
layer-4
layer-5
layer_with_weights-3
layer-6
	optimizer
	
signatures
#
_self_saveable_object_factories
	variables
trainable_variables
regularization_losses
	keras_api
___call__
`_default_save_signature
*a&call_and_return_all_conditional_losses"
_tf_keras_network
D
#_self_saveable_object_factories"
_tf_keras_input_layer
{
_index_lookup_layer
#_self_saveable_object_factories
	keras_api
b_adapt_function"
_tf_keras_layer
?

embeddings
#_self_saveable_object_factories
	variables
trainable_variables
regularization_losses
	keras_api
c__call__
*d&call_and_return_all_conditional_losses"
_tf_keras_layer
?
#_self_saveable_object_factories
	variables
trainable_variables
regularization_losses
	keras_api
e__call__
*f&call_and_return_all_conditional_losses"
_tf_keras_layer
?

kernel
bias
# _self_saveable_object_factories
!	variables
"trainable_variables
#regularization_losses
$	keras_api
g__call__
*h&call_and_return_all_conditional_losses"
_tf_keras_layer
?
#%_self_saveable_object_factories
&	variables
'trainable_variables
(regularization_losses
)	keras_api
i__call__
*j&call_and_return_all_conditional_losses"
_tf_keras_layer
?

*kernel
+bias
#,_self_saveable_object_factories
-	variables
.trainable_variables
/regularization_losses
0	keras_api
k__call__
*l&call_and_return_all_conditional_losses"
_tf_keras_layer
"
	optimizer
,
mserving_default"
signature_map
 "
trackable_dict_wrapper
C
1
2
3
*4
+5"
trackable_list_wrapper
C
0
1
2
*3
+4"
trackable_list_wrapper
 "
trackable_list_wrapper
?
1layer_metrics
2layer_regularization_losses
	variables
3non_trainable_variables
trainable_variables
4metrics

5layers
regularization_losses
___call__
`_default_save_signature
*a&call_and_return_all_conditional_losses
&a"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_dict_wrapper
q
6lookup_table
7token_counts
#8_self_saveable_object_factories
9	keras_api"
_tf_keras_layer
 "
trackable_dict_wrapper
"
_generic_user_object
*:(	?N2embedding_15/embeddings
 "
trackable_dict_wrapper
'
0"
trackable_list_wrapper
'
0"
trackable_list_wrapper
 "
trackable_list_wrapper
?
:layer_metrics
;layer_regularization_losses
	variables
<non_trainable_variables
trainable_variables
=metrics

>layers
regularization_losses
c__call__
*d&call_and_return_all_conditional_losses
&d"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
?layer_metrics
@layer_regularization_losses
	variables
Anon_trainable_variables
trainable_variables
Bmetrics

Clayers
regularization_losses
e__call__
*f&call_and_return_all_conditional_losses
&f"call_and_return_conditional_losses"
_generic_user_object
!:@2dense_40/kernel
:@2dense_40/bias
 "
trackable_dict_wrapper
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
Dlayer_metrics
Elayer_regularization_losses
!	variables
Fnon_trainable_variables
"trainable_variables
Gmetrics

Hlayers
#regularization_losses
g__call__
*h&call_and_return_all_conditional_losses
&h"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
Ilayer_metrics
Jlayer_regularization_losses
&	variables
Knon_trainable_variables
'trainable_variables
Lmetrics

Mlayers
(regularization_losses
i__call__
*j&call_and_return_all_conditional_losses
&j"call_and_return_conditional_losses"
_generic_user_object
!:@2dense_39/kernel
:2dense_39/bias
 "
trackable_dict_wrapper
.
*0
+1"
trackable_list_wrapper
.
*0
+1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
Nlayer_metrics
Olayer_regularization_losses
-	variables
Pnon_trainable_variables
.trainable_variables
Qmetrics

Rlayers
/regularization_losses
k__call__
*l&call_and_return_all_conditional_losses
&l"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
.
S0
T1"
trackable_list_wrapper
Q
0
1
2
3
4
5
6"
trackable_list_wrapper
R
U_initializer
n_create_resource
o_initialize
p_destroy_resourceR 
O
q_create_resource
r_initialize
s_destroy_resourceR Z
tabletu
 "
trackable_dict_wrapper
"
_generic_user_object
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
N
	Vtotal
	Wcount
X	variables
Y	keras_api"
_tf_keras_metric
^
	Ztotal
	[count
\
_fn_kwargs
]	variables
^	keras_api"
_tf_keras_metric
"
_generic_user_object
:  (2total
:  (2count
.
V0
W1"
trackable_list_wrapper
-
X	variables"
_generic_user_object
:  (2total
:  (2count
 "
trackable_dict_wrapper
.
Z0
[1"
trackable_list_wrapper
-
]	variables"
_generic_user_object
?2?
(__inference_model_17_layer_call_fn_16987
(__inference_model_17_layer_call_fn_17359
(__inference_model_17_layer_call_fn_17382
(__inference_model_17_layer_call_fn_17181?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?B?
 __inference__wrapped_model_16662input_8"?
???
FullArgSpec
args? 
varargsjargs
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
C__inference_model_17_layer_call_and_return_conditional_losses_17615
C__inference_model_17_layer_call_and_return_conditional_losses_17855
C__inference_model_17_layer_call_and_return_conditional_losses_17246
C__inference_model_17_layer_call_and_return_conditional_losses_17311?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
__inference_adapt_step_14310?
???
FullArgSpec
args?

jiterator
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
,__inference_embedding_15_layer_call_fn_17865?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
G__inference_embedding_15_layer_call_and_return_conditional_losses_17876?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
;__inference_global_average_pooling1d_15_layer_call_fn_17881
;__inference_global_average_pooling1d_15_layer_call_fn_17887?
???
FullArgSpec%
args?
jself
jinputs
jmask
varargs
 
varkw
 
defaults?

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_17893
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_18062?
???
FullArgSpec%
args?
jself
jinputs
jmask
varargs
 
varkw
 
defaults?

 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
(__inference_dense_40_layer_call_fn_18071?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
C__inference_dense_40_layer_call_and_return_conditional_losses_18082?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
*__inference_dropout_15_layer_call_fn_18087
*__inference_dropout_15_layer_call_fn_18092?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
E__inference_dropout_15_layer_call_and_return_conditional_losses_18097
E__inference_dropout_15_layer_call_and_return_conditional_losses_18109?
???
FullArgSpec)
args!?
jself
jinputs

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
(__inference_dense_39_layer_call_fn_18118?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
C__inference_dense_39_layer_call_and_return_conditional_losses_18129?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
#__inference_signature_wrapper_17336input_8"?
???
FullArgSpec
args? 
varargs
 
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
__inference__creator_18139?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?2?
__inference__initializer_18161?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?2?
__inference__destroyer_18172?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?2?
__inference__creator_18182?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?2?
__inference__initializer_18192?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?2?
__inference__destroyer_18203?
???
FullArgSpec
args? 
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *? 
?B?
__inference_save_fn_18222checkpoint_key"?
???
FullArgSpec
args?
jcheckpoint_key
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *?	
? 
?B?
__inference_restore_fn_18230restored_tensors_0restored_tensors_1"?
???
FullArgSpec
args? 
varargsjrestored_tensors
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *?
	?
	?	
	J
Const
J	
Const_1
J	
Const_2
J	
Const_3
J	
Const_4
J	
Const_56
__inference__creator_18139?

? 
? "? 6
__inference__creator_18182?

? 
? "? 8
__inference__destroyer_18172?

? 
? "? 8
__inference__destroyer_18203?

? 
? "? ?
__inference__initializer_181616z{?

? 
? "? :
__inference__initializer_18192?

? 
? "? ?
 __inference__wrapped_model_16662r	6vwx*+0?-
&?#
!?
input_8?????????
? "3?0
.
dense_39"?
dense_39?????????g
__inference_adapt_step_14310G7y=?:
3?0
.?+?
??????????IteratorSpec
? "
 ?
C__inference_dense_39_layer_call_and_return_conditional_losses_18129\*+/?,
%?"
 ?
inputs?????????@
? "%?"
?
0?????????
? {
(__inference_dense_39_layer_call_fn_18118O*+/?,
%?"
 ?
inputs?????????@
? "???????????
C__inference_dense_40_layer_call_and_return_conditional_losses_18082\/?,
%?"
 ?
inputs?????????
? "%?"
?
0?????????@
? {
(__inference_dense_40_layer_call_fn_18071O/?,
%?"
 ?
inputs?????????
? "??????????@?
E__inference_dropout_15_layer_call_and_return_conditional_losses_18097\3?0
)?&
 ?
inputs?????????@
p 
? "%?"
?
0?????????@
? ?
E__inference_dropout_15_layer_call_and_return_conditional_losses_18109\3?0
)?&
 ?
inputs?????????@
p
? "%?"
?
0?????????@
? }
*__inference_dropout_15_layer_call_fn_18087O3?0
)?&
 ?
inputs?????????@
p 
? "??????????@}
*__inference_dropout_15_layer_call_fn_18092O3?0
)?&
 ?
inputs?????????@
p
? "??????????@?
G__inference_embedding_15_layer_call_and_return_conditional_losses_17876?X?U
N?K
I?F0?-
???????????????????
?	
`
?	RaggedTensorSpec
? "W?T
M?J4?1
!???????????????????
?
`
?	RaggedTensorSpec
? ?
,__inference_embedding_15_layer_call_fn_17865?X?U
N?K
I?F0?-
???????????????????
?	
`
?	RaggedTensorSpec
? "M?J4?1
!???????????????????
?
`
?	RaggedTensorSpec?
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_17893{I?F
??<
6?3
inputs'???????????????????????????

 
? ".?+
$?!
0??????????????????
? ?
V__inference_global_average_pooling1d_15_layer_call_and_return_conditional_losses_18062?`?]
V?S
M?J4?1
!???????????????????
?
`
?	RaggedTensorSpec

 
? "%?"
?
0?????????
? ?
;__inference_global_average_pooling1d_15_layer_call_fn_17881nI?F
??<
6?3
inputs'???????????????????????????

 
? "!????????????????????
;__inference_global_average_pooling1d_15_layer_call_fn_17887|`?]
V?S
M?J4?1
!???????????????????
?
`
?	RaggedTensorSpec

 
? "???????????
C__inference_model_17_layer_call_and_return_conditional_losses_17246l	6vwx*+8?5
.?+
!?
input_8?????????
p 

 
? "%?"
?
0?????????
? ?
C__inference_model_17_layer_call_and_return_conditional_losses_17311l	6vwx*+8?5
.?+
!?
input_8?????????
p

 
? "%?"
?
0?????????
? ?
C__inference_model_17_layer_call_and_return_conditional_losses_17615k	6vwx*+7?4
-?*
 ?
inputs?????????
p 

 
? "%?"
?
0?????????
? ?
C__inference_model_17_layer_call_and_return_conditional_losses_17855k	6vwx*+7?4
-?*
 ?
inputs?????????
p

 
? "%?"
?
0?????????
? ?
(__inference_model_17_layer_call_fn_16987_	6vwx*+8?5
.?+
!?
input_8?????????
p 

 
? "???????????
(__inference_model_17_layer_call_fn_17181_	6vwx*+8?5
.?+
!?
input_8?????????
p

 
? "???????????
(__inference_model_17_layer_call_fn_17359^	6vwx*+7?4
-?*
 ?
inputs?????????
p 

 
? "???????????
(__inference_model_17_layer_call_fn_17382^	6vwx*+7?4
-?*
 ?
inputs?????????
p

 
? "??????????y
__inference_restore_fn_18230Y7K?H
A?>
?
restored_tensors_0
?
restored_tensors_1	
? "? ?
__inference_save_fn_18222?7&?#
?
?
checkpoint_key 
? "???
`?]

name?
0/name 
#

slice_spec?
0/slice_spec 

tensor?
0/tensor
`?]

name?
1/name 
#

slice_spec?
1/slice_spec 

tensor?
1/tensor	?
#__inference_signature_wrapper_17336}	6vwx*+;?8
? 
1?.
,
input_8!?
input_8?????????"3?0
.
dense_39"?
dense_39?????????