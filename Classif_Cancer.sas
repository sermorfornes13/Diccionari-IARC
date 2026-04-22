/* Classification of cancer sites based on ICD-O-3 */
/* Lip, oral cavity and pharynx */
Cncr_Oropharynx = 0 ;
If Site3 in ('C00','C01','C02','C03','C04','C05','C06','C07','C08','C09','C10','C11','C12','C13','C14') Then Cncr_Oropharynx = 1 ;
Label Cncr_Oropharynx = 'Lip, oral cavity and pharynx cancer' ;

/* Esophagus */
Cncr_Esophagus = 0 ;
If Site3 in ('C15') Then Cncr_Esophagus = 1 ;
Label Cncr_Esophagus = 'Esophagus cancer' ;

/* Stomach */
Cncr_Stomach = 0 ;
If Site3 in ('C16') Then Cncr_Stomach = 1 ;
Label Cncr_Stomach = 'Stomach cancer' ;

/* Small intestine */
Cncr_Small_Intestine = 0 ;
If Site3 in ('C17') Then Cncr_Small_Intestine = 1 ;
Label Cncr_Small_Intestine = 'Small intestine cancer' ;

/* Colon */
Cncr_Colon = 0 ;
If Site3 in ('C18') Then Cncr_Colon = 1 ;
Label Cncr_Colon = 'Colon cancer' ;

/* Rectal */
Cncr_Rectal = 0 ;
If Site3 in ('C19','C20') Then Cncr_Rectal = 1 ;
Label Cncr_Rectal = 'Rectum cancer' ;

/* Anus */
Cncr_Anus = 0 ;
If Site3 in ('C21') Then Cncr_Anus = 1 ;
Label Cncr_Anus = 'Anus cancer' ;

/* Liver */
Cncr_Liver = 0 ;
If Site3 in ('C22') Then Cncr_Liver = 1 ;
Label Cncr_Liver = 'Liver cancer' ;

/* Gallbladder and other and unspecified parts of biliary tract */
Cncr_Gallb = 0 ;
If Site3 in ('C23','C24') Then Cncr_Gallb = 1 ;
Label Cncr_Gallb = 'Gallbladder and biliary tract cancer' ;

/* Pancreas */
Cncr_Pancreas = 0 ;
If Site3 in ('C25') Then Cncr_Pancreas = 1 ;
Label Cncr_Pancreas = 'Pancreas cancer' ;

/* Unknown and ill-defined digestive organs */
Cncr_Unknown_Dig = 0 ;
If Site3 in ('C26') Then Cncr_Unknown_Dig = 1 ;
Label Cncr_Unknown_Dig = 'Unknown and ill-defined digestive organs cancer' ;

/* Nose and sinuses */
Cncr_Nose_Sinuses = 0 ;
If Site3 in ('C30','C31') Then Cncr_Nose_Sinuses = 1 ;
Label Cncr_Nose_Sinuses = 'Nose and sinuses cancer' ;

/* Larynx */
Cncr_Larynx = 0 ;
If Site3 in ('C32') Then Cncr_Larynx = 1 ;
Label Cncr_Larynx = 'Larynx cancer' ;

/* Lung Cancer (trachea, bronchus and lung) */
Cncr_Lung = 0 ;
If Site3 in ('C33','C34') Then Cncr_Lung = 1 ;
Label Cncr_Lung = 'Lung cancer' ;

/* Thymus */
Cncr_Thymus = 0 ;
If Site3 in ('C37') Then Cncr_Thymus = 1 ;
Label Cncr_Thymus = 'Thymus cancer' ;

/* Heart, mediastinum and pleura */
Cncr_Pleura = 0 ;
If Site3 in ('C38') Then Cncr_Pleura = 1 ;
Label Cncr_Pleura = 'Heart, pleura and mediastinum cancer' ;

/* Other and ill-defined sites within respiratory system amd intrathoracic organs */
Cncr_Unknown_Resp = 0 ;
If Site3 in ('C39') Then Cncr_Unknown_Resp = 1 ;
Label Cncr_Unknown_Resp = 'Unknown and ill-defined sites within respiratory system cancer' ;

/* Bones */
Cncr_Bone = 0 ;
If Site3 in ('C40','C41') Then Cncr_Bone = 1 ;
Label Cncr_Bone = 'Bones cancer' ;

/* Myeloma/hematopoietic and reticuloendothelial systems */
Cncr_Myeloma = 0 ;
If Site3 in ('C42') Then Cncr_Myeloma = 1 ;
Label Cncr_Myeloma = 'Myeloma cancer' ;

/* Skin */
Cncr_Skin = 0 ;
If Site3 in ('C44') Then Cncr_Skin = 1 ;
Label Cncr_Skin = 'Skin cancer' ;

/* Peripheral nerves and autonomic nervous system */
Cncr_Peripheral_Nerves = 0 ;
If Site3 in ('C47') Then Cncr_Peripheral_Nerves = 1 ;
Label Cncr_Peripheral_Nerves = 'Peripheral nerves and automatic nervous system cancer' ;

/* Retroperitoneum and peritoneum */
Cncr_Peritoneum = 0 ;
If Site3 in ('C48') Then Cncr_Peritoneum = 1 ;
Label Cncr_Peritoneum = 'Retroperitoneum and peritoneum cancer' ;

/* Soft tissues */
Cncr_Soft_Tissues = 0 ;
If Site3 in ('C49') Then Cncr_Soft_Tissues = 1 ;
Label Cncr_Soft_Tissues = 'Soft tissues cancer' ;

/* Breast */
Cncr_Breast = 0 ;
If Site3 in ('C50') Then Cncr_Breast = 1 ;
Label Cncr_Breast = 'Breast cancer' ;

/* Vulva and vagina */
Cncr_Vulva = 0 ;
If Site3 in ('C51','C52') Then Cncr_Vulva = 1 ;
Label Cncr_Vulva = 'Vulva and vagina cancer' ;

/* Cervical Cancer */
Cncr_Cervical = 0 ;
If Site3 in ('C53') Then Cncr_Cervical = 1 ;
Label Cncr_Cervical = 'Cervical cancer' ;

/* Endometrium */
Cncr_Endom = 0 ;
If Site3 in ('C54') Then Cncr_Endom = 1 ;
Label Cncr_Endom = 'Endometrium cancer' ;

/* Uterus */
Cncr_Uterus = 0 ;
If Site3 in ('C55') Then Cncr_Uterus = 1 ;
Label Cncr_Uterus = 'Uterus cancer' ;

/* Ovary */
Cncr_Ovary = 0 ;
If Site3 in ('C56') Then Cncr_Ovary = 1 ;
Label Cncr_Ovary = 'Ovary cancer' ;

/* Other and unspecified female genital organs */
Cncr_Fem_Gen = 0 ;
If Site3 in ('C57') Then Cncr_Fem_Gen = 1 ;
Label Cncr_Fem_Gen = 'Other female genital organs cancer' ;

/* Placenta */
Cncr_Placenta = 0 ;
If Site3 in ('C58') Then Cncr_Placenta = 1 ;
Label Cncr_Placenta = 'Placenta cancer' ;

/* Penis */
Cncr_Penis = 0 ;
If Site3 in ('C60') Then Cncr_Penis = 1 ;
Label Cncr_Penis = 'Penis cancer' ;

/* Prostate */
Cncr_Prostate = 0 ;
If Site3 in ('C61') Then Cncr_Prostate = 1 ;
Label Cncr_Prostate = 'Prostate cancer' ;

/* Testis */
Cncr_Testis = 0 ;
If Site3 in ('C62') Then Cncr_Testis = 1 ;
Label Cncr_Testis = 'Testis cancer' ;

/* Other and unspecified male genital organs */
Cncr_Male_Gen = 0 ;
If Site3 in ('C63') Then Cncr_Male_Gen = 1 ;
Label Cncr_Male_Gen = 'Other male genital organs cancer' ;

/* Kidney */
Cncr_Kidney = 0 ;
If Site3 in ('C64') Then Cncr_Kidney = 1 ;
Label Cncr_Kidney = 'Kidney cancer' ;

/* Bladder, renal pelvis, ureter and other and unspecified urinary organs */
Cncr_Bladder = 0 ;
If Site3 in ('C65','C66','C67','C68') Then Cncr_Bladder = 1 ;
Label Cncr_Bladder = 'Bladder, renal pelvis, ureter and other urinary organs cancer' ;

/* Eye */
Cncr_Eye = 0 ;
If Site3 in ('C69') Then Cncr_Eye = 1 ;
Label Cncr_Eye = 'Eye cancer' ;

/* Brain, central nervous system */
Cncr_Brain = 0 ;
If Site3 in ('C70','C71','C72') Then Cncr_Brain = 1 ;
Label Cncr_Brain = 'Brain and central nervous system cancer' ;

/* Thyroid */
Cncr_Thyroid = 0 ;
If Site3 in ('C73') Then Cncr_Thyroid = 1 ;
Label Cncr_Thyroid = 'Thyroid cancer' ;

/* Adrenal gland */
Cncr_Adrenal = 0 ;
If Site3 in ('C74') Then Cncr_Adrenal = 1 ;
Label Cncr_Adrenal = 'Adrenal gland cancer' ;

/* Other endocrine glands and related structures */
Cncr_Endocrine_Gland = 0 ;
If Site3 in ('C75') Then Cncr_Endocrine_Gland = 1 ;
Label Cncr_Endocrine_Gland = 'Other endocrine glands cancer' ;

/* Other and ill-defined sites */
Cncr_Ill_Def = 0 ;
If Site3 in ('C76') Then Cncr_Ill_Def = 1 ;
Label Cncr_Ill_Def = 'Other and ill-defined sites cancer' ;

/* Lymph nodes */
Cncr_Lymph_Nodes = 0 ;
If Site3 in ('C77') Then Cncr_Lymph_Nodes = 1 ;
Label Cncr_Lymph_Nodes = 'Lymph nodes cancer' ;

/* Unknown primary site */
Cncr_Unknown_Site = 0 ;
If Site3 in ('C80','') Then Cncr_Unknown_Site = 1 ;
Label Cncr_Unknown_Site = 'Unknown primary site cancer' ;

/* Check there is no missing or overlap */
If Sum(Cncr_Oropharynx, Cncr_Esophagus, Cncr_Stomach, Cncr_Small_Intestine, Cncr_Colon, Cncr_Rectal, Cncr_Anus, Cncr_Liver, Cncr_Gallb, Cncr_Pancreas,
	Cncr_Unknown_Dig, Cncr_Nose_Sinuses, Cncr_Larynx, Cncr_Lung, Cncr_Thymus, Cncr_Pleura, Cncr_Unknown_Resp, Cncr_Bone, Cncr_Myeloma, Cncr_Skin,
	Cncr_Peripheral_Nerves, Cncr_Peritoneum, Cncr_Soft_Tissues, Cncr_Breast, Cncr_Vulva, Cncr_Cervical, Cncr_Endom, Cncr_Uterus, Cncr_Ovary, Cncr_Fem_Gen,
	Cncr_Placenta, Cncr_Penis, Cncr_Prostate, Cncr_Testis, Cncr_Male_Gen, Cncr_Kidney, Cncr_Bladder, Cncr_Eye, Cncr_Brain, Cncr_Thyroid, Cncr_Adrenal,
	Cncr_Endocrine_Gland, Cncr_Ill_Def, Cncr_Lymph_Nodes, Cncr_Unknown_Site) ne 1 Then Put 'ERROR on cancer classification' Idepic= Sit_Tumo= $4. ;

Format Cncr_Oropharynx Cncr_Esophagus Cncr_Stomach Cncr_Small_Intestine Cncr_Colon Cncr_Rectal Cncr_Anus Cncr_Liver Cncr_Gallb Cncr_Pancreas
	Cncr_Unknown_Dig Cncr_Nose_Sinuses Cncr_Larynx Cncr_Lung Cncr_Thymus Cncr_Pleura Cncr_Unknown_Resp Cncr_Bone Cncr_Myeloma Cncr_Skin
	Cncr_Peripheral_Nerves Cncr_Peritoneum Cncr_Soft_Tissues Cncr_Breast Cncr_Vulva Cncr_Cervical Cncr_Endom Cncr_Uterus Cncr_Ovary Cncr_Fem_Gen
	Cncr_Placenta Cncr_Penis Cncr_Prostate Cncr_Testis Cncr_Male_Gen Cncr_Kidney Cncr_Bladder Cncr_Eye Cncr_Brain Cncr_Thyroid Cncr_Adrenal
	Cncr_Endocrine_Gland Cncr_Ill_Def Cncr_Lymph_Nodes Cncr_Unknown_Site Yes_No. ;

/* Sub-classification of cancer sites */
/* Brain */
Cncr_Brain_Meningioma = 0 ;
Cncr_Brain_Glioma = 0 ;
Cncr_Brain_Others = 0 ;
If Cncr_Brain eq 1 Then Do ;
	If Sit_Tumo in ('C700','C701','C709') Then Cncr_Brain_Meningioma = 1 ;
	Else If Site3 eq 'C71' and
		(Substr(Mor_Tumo_Icdo3,1,3) in ('938','939','940','941','942','943','944','945','946','947','948') or Substr(Mor_Tumo_Icdo3,1,4) eq '9505') Then Cncr_Brain_Glioma = 1 ;
	Else If Mor_Tumo_Icdo3 ne '' Then Cncr_Brain_Others = 1 ;
End ;
Label Cncr_Brain_Meningioma = 'Brain and CNS cancer-Meningioma'
			Cncr_Brain_Glioma = 'Brain and CNS cancer-Glioma'
			Cncr_Brain_Others = 'Brain and CNS cancer-Others' ;
Format Cncr_Brain_Meningioma Cncr_Brain_Glioma Cncr_Brain_Others Yes_No. ;

/* Colorectal */
Cncr_Colon_Prox = 0 ;
Cncr_Colon_Dist = 0 ;
Cncr_Colon_Nos = 0 ;
Cncr_Colorectal = 0 ;
If Cncr_Colon eq 1 Then Do ;
	If Sit_Tumo in ('C180','C181','C182','C183','C184','C185') Then Cncr_Colon_Prox = 1 ;
	Else If Sit_Tumo in ('C186','C187') Then Cncr_Colon_Dist = 1 ;
	Else If Sit_Tumo in ('C188','C189') Then Cncr_Colon_Nos = 1 ;
End ;
If Cncr_Colon eq 1 or Cncr_Rectal eq 1 Then Cncr_Colorectal = 1 ;
Label Cncr_Colon_Prox = 'Colon cancer-Proximal'
			Cncr_Colon_Dist = 'Colon cancer-Distal'
			Cncr_Colon_Nos = 'Colon cancer-Overlapping/Nos'
			Cncr_Colorectal = 'Colorectal cancer' ;
Format Cncr_Colon_Prox Cncr_Colon_Dist Cncr_Colon_Nos Cncr_Colorectal Yes_No. ;

/* Esophagus */
Cncr_Esophagus_Scc = 0 ;
Cncr_Esophagus_Adeno = 0 ;
Cncr_Esophagus_Oth_Carci = 0 ;
Cncr_Esophagus_Others = 0 ;
If Cncr_Esophagus eq 1 Then Do ;
	If Morpnum in (8070,8071,8072,8073,8074,8076,8078) Then Cncr_Esophagus_Scc = 1 ;
	Else If Morpnum in (8140,8144,8480,8481,8490) Then Cncr_Esophagus_Adeno = 1 ;
	Else If Morpnum in (8010,8012,8020,8230,8240,8241,8560) Then Cncr_Esophagus_Oth_Carci = 1 ;
	Else If Morpnum in (8000,8041,8246,8170,8500,8720,8890,9591,9680) Then Cncr_Esophagus_Others = 1 ;
End ;
Label Cncr_Esophagus_Scc = 'Esophagus cancer-Squamous cell carcinoma'
			Cncr_Esophagus_Adeno = 'Esophagus cancer-Adenocarcinoma'
			Cncr_Esophagus_Oth_Carci = 'Esophagus cancer-Other carcinoma'
			Cncr_Esophagus_Others = 'Esophagus cancer-Others' ;
Format Cncr_Esophagus_Scc Cncr_Esophagus_Adeno Cncr_Esophagus_Oth_Carci Cncr_Esophagus_Others Yes_No. ;

/* Liver/Gallbladder */
Cncr_Liver_HCC = 0 ;
Cncr_Liver_IBD = 0 ;
Cncr_Liver_Others = 0 ;
Cncr_Gallb_Gallblad = 0 ;
Cncr_Gallb_EBD = 0 ;
Cncr_Gallb_AOV = 0 ;
Cncr_Gallb_Others = 0 ;
If Cncr_Liver eq 1 Then Do ;
	/* Cncr_Liver_HCC: Hepatocellular carcinoma (definite) */
	If Sit_Tumo eq 'C220' and Mor_Tumo_Icdo3 in ('8170/3','8171/3','8180/3') Then Cncr_Liver_HCC = 1 ;
	Else If Sit_Tumo eq 'C221' and Mor_Tumo_Icdo3 in ('8170/3') Then Cncr_Liver_HCC = 1 ;
	/* Cncr_Liver_IBD : Intrahepatic bile duct */
	Else If Sit_Tumo eq 'C220' and Mor_Tumo_Icdo3 in ('8160/3','8211/3','8481/3') Then Cncr_Liver_IBD = 1 ;
	Else If Sit_Tumo eq 'C221' and Mor_Tumo_Icdo3 in ('8000/3','8160/3','8161/3','8211/3','8260/3') Then Cncr_Liver_IBD = 1 ;
	Else If Mor_Tumo_Icdo3 ne '' Then Cncr_Liver_Others = 1 ;
End ;
If Cncr_Gallb eq 1 Then Do ;
	/* Cncr_Gallb_Gallblad : Gallbladder */
	If Sit_Tumo eq 'C239' and Mor_Tumo_Icdo3 in ('8000/3','8010/2','8010/3','8020/3','8140/3','8160/3','8260/3','8480/3','8490/3','8560/3') Then Cncr_Gallb_Gallblad = 1 ;
	/* Cncr_Gallb_EBD : Extrahepatic bile duct */
	Else If Sit_Tumo eq 'C240' and Mor_Tumo_Icdo3 in ('8000/3','8010/3','8140/3','8160/3','8162/3','8260/3') Then Cncr_Gallb_EBD = 1 ;
	/* Cncr_Gallb_AOV : Ampulla of Vater */
	Else If Sit_Tumo eq 'C241' and Mor_Tumo_Icdo3 in ('8000/3','8010/3','8140/3','8260/3','8480/3','8481/3','8490/3') Then Cncr_Gallb_AOV = 1 ;
	Else If Mor_Tumo_Icdo3 ne '' Then Cncr_Gallb_Others = 1 ;
End ;
Label Cncr_Liver_HCC = 'Liver cancer-Hepatocellular carcinoma'
			Cncr_Liver_IBD = 'Liver cancer-Intrahepatic bile duct'
			Cncr_Liver_Others = 'Liver cancer-Others'
			Cncr_Gallb_Gallblad = 'Gallbladder cancer-Gallbladder'
			Cncr_Gallb_EBD = 'Gallbladder cancer-Extrahepatic bile duct'
			Cncr_Gallb_AOV = 'Gallbladder cancer-Ampulla of Vater'
			Cncr_Gallb_Others = 'Gallbladder cancer-Others' ;
Format Cncr_Liver_HCC Cncr_Liver_IBD Cncr_Liver_Others Cncr_Gallb_Gallblad Cncr_Gallb_EBD Cncr_Gallb_AOV Cncr_Gallb_Others Yes_No. ;

/* Prostate */
Cncr_Prostate_Low_Grade = 0 ;
Cncr_Prostate_High_Grade = 0 ;
Cncr_Prostate_Localised = 0 ;
Cncr_Prostate_Advanced = 0 ;
Cncr_Prostate_Advanced_7 = 0 ;
Cncr_Prostate_Advanced_8 = 0 ;
If Cncr_Prostate eq 1 Then Do ;
	/* Prostate low/high grade */
	If Glea_Score ge 8 or Gra_Tumo eq 4 Then Cncr_Prostate_High_Grade = 1 ;
	Else If Glea_Score in (1,2,3,4,5,6,7) or Gra_Tumo in (1,2,3) Then Cncr_Prostate_Low_Grade = 1 ;
	/* Prostate localised/advanced stage */
	If (Substr(Tnm_Tumo,2,2) in ('T3','T4') or Substr(Tnm_Tumo,5,2) in ('N1','N2','N3')
		or Substr(Tnm_Tumo,8,2) in ('M1')) Then Tnmtype = 2 ;
	Else If (Substr(Tnm_Tumo,2,2) in ('T0','T1','T2') and Substr(Tnm_Tumo,5,2) in ('N0','NX')
				and Substr(Tnm_Tumo,8,2) in ('M0')) Then Tnmtype = 1 ;
	Else Tnmtype = 9 ;
	If (Tnmtype eq 1 or (Sta_Tumo in (1,2) and Tnmtype eq 9) or (Summary_Stage in (1) and Tnmtype eq 9)) Then Cncr_Prostate_Localised = 1 ;
	If (Tnmtype eq 2 or (Sta_Tumo in (3,4,5) and Tnmtype eq 9) or (Summary_Stage in (2,3,4) and Tnmtype eq 9)) Then Cncr_Prostate_Advanced = 1 ;
	/* Prostate Aggressive */
	If Cncr_Prostate_Advanced eq 1 or Glea_Score ge 7 Then Cncr_Prostate_Advanced_7 = 1 ;
	If Cncr_Prostate_Advanced eq 1 or Glea_Score ge 8 Then Cncr_Prostate_Advanced_8 = 1 ;
End ;
Label Cncr_Prostate_Low_Grade = 'Prostate cancer-Low grade'
			Cncr_Prostate_High_Grade = 'Prostate cancer-High grade'
			Cncr_Prostate_Localised = 'Prostate cancer-Localised'
			Cncr_Prostate_Advanced = 'Prostate cancer-Advanced'
			Cncr_Prostate_Advanced_7 = 'Prostate cancer-Aggressive status (Gleason>=7)'
			Cncr_Prostate_Advanced_8 = 'Prostate cancer-Aggressive status (Gleason>=8)' ;
Format Cncr_Prostate_Low_Grade Cncr_Prostate_High_Grade Cncr_Prostate_Localised Cncr_Prostate_Advanced Cncr_Prostate_Advanced_7 Cncr_Prostate_Advanced_8 Yes_No. ;
Drop Tnmtype ;

/* Skin */
Cncr_Skin_Scc = 0 ;
Cncr_Skin_Melanoma = 0 ;
Cncr_Skin_Others = 0 ;
If Cncr_Skin eq 1 Then Do ;
	If Substr(Mor_Tumo_Icdo3,1,3) in ('805','806','807','808') Then Cncr_Skin_Scc = 1 ;
	Else If Substr(Mor_Tumo_Icdo3,1,3) in ('872','873','874','875','876','877','878','879') Then Cncr_Skin_Melanoma = 1 ;
	Else If Mor_Tumo_Icdo3 ne '' Then Cncr_Skin_Others = 1 ;
End ;
Label Cncr_Skin_Scc = 'Skin cancer-Squamous cell carcinoma'
			Cncr_Skin_Melanoma = 'Skin cancer-Melanoma'
			Cncr_Skin_Others = 'Skin cancer-Others' ;
Format Cncr_Skin_Scc Cncr_Skin_Melanoma Cncr_Skin_Others Yes_No. ;

/* Stomach */
Cncr_Stomach_Cardia = 0 ;
Cncr_Stomach_Non_Cardia = 0 ;
Cncr_Stomach_Others = 0 ;
If Cncr_Stomach eq 1 Then Do ;
	If Sit_Tumo eq 'C160' Then Cncr_Stomach_Cardia = 1 ;
	Else If Sit_Tumo in ('C161','C162','C163','C164','C165','C166') Then Cncr_Stomach_Non_Cardia = 1 ;
	Else If Sit_Tumo in ('C168','C169') Then Cncr_Stomach_Others = 1 ;
End ;
Label Cncr_Stomach_Cardia = 'Stomach cancer-Cardia'
			Cncr_Stomach_Non_Cardia = 'Stomach cancer-Non cardia'
			Cncr_Stomach_Others = 'Stomach cancer-Others' ;
Format Cncr_Stomach_Cardia Cncr_Stomach_Non_Cardia Cncr_Stomach_Others Yes_No. ;

/* Thyroid */
Cncr_Thyroid_Papillary = 0 ;
Cncr_Thyroid_Follicular = 0 ;
Cncr_Thyroid_Others = 0 ;
If Cncr_Thyroid eq 1 Then Do ;
	If Morpnum in (8050,8052,8130,8260,8263,8340,8341,8342,8343,8344,8350,8450) Then Cncr_Thyroid_Papillary = 1 ;
	Else If Morpnum in (8290,8330,8331,8332,8333,8334,8335,8480,8490) Then Cncr_Thyroid_Follicular = 1 ;
	Else If Mor_Tumo_Icdo3 ne '' Then Cncr_Thyroid_Others = 1 ;
End ;
Label Cncr_Thyroid_Papillary = 'Thyroid cancer-Papillary'
			Cncr_Thyroid_Follicular = 'Thyroid cancer-Follicular'
			Cncr_Thyroid_Others = 'Thyroid cancer-Others' ;
Format Cncr_Thyroid_Papillary Cncr_Thyroid_Follicular Cncr_Thyroid_Others Yes_No. ;

/* Add lymphomas */
Cncr_Lymphoma = 0 ;
Cncr_Lymphoma_Malignant = 0 ;
Cncr_Lymphoma_Hodgkin = 0 ;
Cncr_Lymphoma_Non_Hodgkin = 0 ;
If (Substr(Mor_Tumo_Icdo3,1,3) in ('959','965','966','967','968','969','970','971','973','976','982','983') or
		Substr(Mor_Tumo_Icdo3,1,4) in ('9940','9970') ) Then Cncr_Lymphoma = 1 ;
If Cncr_Lymphoma eq 1 Then Do ;
	If Substr(Mor_Tumo_Icdo3,1,3) in ('959') Then Cncr_Lymphoma_Malignant = 1 ;
	If Substr(Mor_Tumo_Icdo3,1,3) in ('965','966') Then Cncr_Lymphoma_Hodgkin = 1 ;
	If Substr(Mor_Tumo_Icdo3,1,3) in ('967','968','969','970','971','973','976','982','983','994','997') Then Cncr_Lymphoma_Non_Hodgkin = 1 ;
End ;
Label Cncr_Lymphoma = 'Lymphoma cancer'
			Cncr_Lymphoma_Malignant = 'Lymphoma cancer-Malignant'
			Cncr_Lymphoma_Hodgkin = 'Lymphoma cancer-Hodgkin'
			Cncr_Lymphoma_Non_Hodgkin = 'Lymphoma cancer-NHL' ;
Format Cncr_Lymphoma Cncr_Lymphoma_Malignant Cncr_Lymphoma_Hodgkin Cncr_Lymphoma_Non_Hodgkin Yes_No. ;
