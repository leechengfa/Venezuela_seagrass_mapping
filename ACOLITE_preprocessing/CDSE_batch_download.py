from cdsetool.query import query_features, shape_to_wkt
from cdsetool.credentials import Credentials
# from cdsetool.credentials import validate_credentials
from cdsetool.query import describe_collection
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

search_terms = describe_collection("Sentinel2").keys()
print(search_terms)


features = query_features(
    "Sentinel2",
    {
        "startDate": "2023-01-16",
        "completionDate": "2023-01-31",
        "processingLevel": "S2MSI1C",
        # 'processingBaseline': '05.09',
        "geometry": shape_to_wkt(shapefile_name),
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


# # ## =====
# alternate version used, when there is a time out caused by a time range that's too big

# features = query_features(
#     "Sentinel2",
#     {
#         "startDate": "2023-03-01",
#         "completionDate": "2023-04-30",
#         "processingLevel": "S2MSI1C",
#         'processingBaseline': '05.09',
#         "geometry": shape_to_wkt(shapefile_name),
#     },
# )

# list(
#     download_features(
#         features,
#         "Images_2023/",
#         {
#             "concurrency": 4,
#             "monitor": StatusMonitor(),
#             "credentials": Credentials(username, password),
#         },
#     )
# )

