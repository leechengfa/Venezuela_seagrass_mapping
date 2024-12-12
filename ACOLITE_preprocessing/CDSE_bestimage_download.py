from cdsetool.query import query_features, shape_to_wkt
from cdsetool.credentials import Credentials
# from cdsetool.credentials import validate_credentials
# from cdsetool.query import describe_collection
# from cdsetool.query import _get_describe_doc
from cdsetool.download import download_features
from cdsetool.monitor import StatusMonitor
# from cdsetool.monitor import NoopMonitor
from datetime import date

## read here for options
## https://catalogue.dataspace.copernicus.eu/resto/api/collections/Sentinel2/describe.xml 

## get ready to update processing baseline for images after 25 January 2022
## https://sentinels.copernicus.eu/web/sentinel/technical-guides/sentinel-2-msi/processing-baseline 


shapefile_name = "Venezuela_AOI_coarse.shp"

## generated list of search terms 
# dict_keys(['maxRecords', 'index', 'page', 'identifier', 'geometry', 'box', 'lon', 'lat', 'radius', 'startDate', 'completionDate', 'productIdentifier', 'productType', 'tileId', 'processingLevel', 'platform', 'instrument', 'orbitNumber', 'sensorMode', 'cloudCover', 'updated', 'publishedAfter', 'publishedBefore', 'sortParam', 'sortOrder', 'status', 'exactCount', 'orbitDirection', 'relativeOrbitNumber', 'processingBaseline', 'missionTakeId'])

features = query_features(
    "Sentinel2",
    {
        "productIdentifier": 'S2A_MSIL1C_20231124T143741_N0509_R096_T20PQQ_20231124T162039'
    },
)

username = "insert_user_name"
password =  "insert_password"

list(
    download_features(
        features,
        "Images_2023/",
        {
            "concurrency": 4,
            "monitor": StatusMonitor(),
            "credentials": Credentials(username, password),
        },
    )
)

list_bestimages = ["S2B_MSIL1C_20231205T145719_N0509_R039_T20PKT_20231205T164035",
                   'S2A_MSIL1C_20230504T145721_N0509_R039_T19PGP_20230504T182025','S2A_MSIL1C_20230504T145721_N0509_R039_T19PHP_20230504T182025',
                   'S2A_MSIL1C_20230911T145731_N0509_R039_T19PGP_20230911T181824','S2A_MSIL1C_20230911T145731_N0509_R039_T19PHP_20230911T181824',
                   'S2A_MSIL1C_20231120T145721_N0509_R039_T20PLU_20231120T164430','S2A_MSIL1C_20231130T145721_N0509_R039_T20PLU_20231130T163853',
                   'S2B_MSIL1C_20231013T144729_N0509_R139_T20PLT_20231013T174926','S2B_MSIL1C_20231013T144729_N0509_R139_T20PLS_20231013T174926',
                   'S2B_MSIL1C_20231013T144729_N0509_R139_T20PMT_20231013T174926','S2B_MSIL1C_20231013T144729_N0509_R139_T20PMS_20231013T174926',
                   'S2B_MSIL1C_20231202T144729_N0509_R139_T20PNS_20231202T163002','S2A_MSIL1C_20230128T143721_N0509_R096_T20PNS_20230128T175812',
                   'S2A_MSIL1C_20230128T143721_N0509_R096_T20PPR_20230128T175812','S2A_MSIL1C_20230128T143721_N0509_R096_T20PPS_20230128T175812',
                   'S2A_MSIL1C_20231124T143741_N0509_R096_T20PPR_20231124T162039','S2A_MSIL1C_20231124T143741_N0509_R096_T20PQR_20231124T162039',
                   'S2A_MSIL1C_20231124T143741_N0509_R096_T20PQQ_20231124T162039','S2A_MSIL1C_20231124T143741_N0509_R096_T20PQQ_20231124T162039',
                   'S2A_MSIL1C_20231124T143741_N0509_R096_T20PRQ_20231124T162039']

for best in list_bestimages:
    features = query_features(
        "Sentinel2",
        {
            "productIdentifier": best,
        },
    )

    list(
        download_features(
            features,
            "Images_2023/",
            {
                "concurrency": 1,
                "monitor": StatusMonitor(),
                "credentials": Credentials(username, password),
            },
        )
    )
