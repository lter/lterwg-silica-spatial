## Data collection sync system

We decided that it will be easier for participants to upload their data to Google Drive and that we will be using [rclone](https://rclone.org/drive/) to synchronize this specific folder to Aurora.

Path on the WG Google Drive:
`Si_LTER_Synthesis > Analysis`
`Si_LTER_Synthesis > Completed_DataTemplate_Uploads`
`Si_LTER_Synthesis > Master_Dataset`

The folder on Aurora it is syncing to is:  

`/home/shares/lter-si/gdrive`

### Update Aurora's content

The command to update the folder on Aurora from Google Drive: 
 **Should be run inside the `gdrive` folder on Aurora**

`rclone --config="rclone_lter-si.conf" copy lter-si-analysis: Analysis`
`rclone --config="rclone_lter-si.conf" copy lter-si-datatemplate: Completed_DataTemplate_Uploads`
`rclone --config="rclone_lter-si.conf" copy lter-si-masterdataset: Master_Dataset`