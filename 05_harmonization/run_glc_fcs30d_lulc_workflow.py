"""Colab-ready GEE workflow for final LULC target sites.

This script extracts GLC-FCS30D class histograms for the final site list
that currently has spatial data but is still missing LULC harmonization.

Expected asset fields:
- LTER
- Stream_Name

Recommended use:
1. Upload the target watershed polygons to a GEE asset.
2. Set GEE_ASSET_PATH below.
3. Run this script in Colab or another authenticated Earth Engine session.
"""

from pathlib import Path

import ee
import pandas as pd


PROJECT = "ee-keirajohnson"
GEE_ASSET_PATH = "projects/ee-keirajohnson/assets/final_lulc_target_sites_20260518"
TARGET_SITES_CSV = "final_lulc_target_sites_20260518.csv"
DRIVE_OUTPUT_DIR = "/content/drive/My Drive/WRLF_GEE_Data"
ANNUAL_OUTPUT_NAME = "GLC_Annual_final35sites.csv"
FIVE_YEAR_OUTPUT_NAME = "GLC_FiveYear_final35sites.csv"
REDUCTION_SCALE_M = 1000
ANNUAL_MAX_PIXELS = 1e13
FIVE_YEAR_MAX_PIXELS = 1e20


ANNUAL_BAND_TO_YEAR = {
    "lcPixelCount_b1": 2000,
    "lcPixelCount_b2": 2001,
    "lcPixelCount_b3": 2002,
    "lcPixelCount_b4": 2003,
    "lcPixelCount_b5": 2004,
    "lcPixelCount_b6": 2005,
    "lcPixelCount_b7": 2006,
    "lcPixelCount_b8": 2007,
    "lcPixelCount_b9": 2008,
    "lcPixelCount_b10": 2009,
    "lcPixelCount_b11": 2010,
    "lcPixelCount_b12": 2011,
    "lcPixelCount_b13": 2012,
    "lcPixelCount_b14": 2013,
    "lcPixelCount_b15": 2014,
    "lcPixelCount_b16": 2015,
    "lcPixelCount_b17": 2016,
    "lcPixelCount_b18": 2017,
    "lcPixelCount_b19": 2018,
    "lcPixelCount_b20": 2019,
    "lcPixelCount_b21": 2020,
    "lcPixelCount_b22": 2021,
    "lcPixelCount_b23": 2022,
}


FIVE_YEAR_BAND_TO_YEAR = {
    "lcPixelCount_b1": 1985,
    "lcPixelCount_b2": 1990,
    "lcPixelCount_b3": 1995,
}


CLASS_TO_LANDTYPE = {
    "10": "Rainfed_cropland",
    "11": "Herbaceous_cover_cropland",
    "12": "Tree_or_shrub_cover_cropland",
    "20": "Irrigated_cropland",
    "51": "Open_evergreen_broadleaved_forest",
    "52": "Closed_evergreen_broadleaved_forest",
    "61": "Open_deciduous_broadleaved_forest",
    "62": "Closed_deciduous_broadleaved_forest",
    "71": "Open_evergreen_needle_leaved_forest",
    "72": "Closed_evergreen_needle_leaved_forest",
    "81": "Open_deciduous_needle_leaved_forest",
    "82": "Closed_deciduous_needle_leaved_forest",
    "91": "Open_mixed_leaf_forest",
    "92": "Closed_mixed_leaf_forest",
    "120": "Shrubland",
    "121": "Evergreen_shrubland",
    "122": "Deciduous_shrubland",
    "130": "Grassland",
    "140": "Lichens_and_mosses",
    "150": "Sparse_vegetation",
    "152": "Sparse_shrubland",
    "153": "Sparse_herbaceous",
    "181": "Swamp",
    "182": "Marsh",
    "183": "Flooded_flat",
    "184": "Saline",
    "185": "Mangrove",
    "186": "Salt_marsh",
    "187": "Tidal_flat",
    "190": "Impervious_surfaces",
    "200": "Bare_areas",
    "201": "Consolidated_bare_areas",
    "202": "Unconsolidated_bare_areas",
    "210": "Water_body",
    "220": "Permanent_ice_and_snow",
    "0": "Filled_value",
}


def authenticate_ee(project: str = PROJECT) -> None:
    try:
        ee.Initialize(project=project)
    except Exception:
        ee.Authenticate()
        ee.Initialize(project=project)


def load_target_sites(csv_path: str) -> list[tuple[str, str]]:
    df = pd.read_csv(csv_path)
    return list(df[["LTER", "Stream_Name"]].itertuples(index=False, name=None))


def build_site_filter(target_sites: list[tuple[str, str]]) -> ee.Filter:
    filters = [
        ee.Filter.And(
            ee.Filter.eq("LTER", lter),
            ee.Filter.eq("Stream_Name", stream_name),
        )
        for lter, stream_name in target_sites
    ]
    return ee.Filter.Or(*filters)


def add_site_id(feature: ee.Feature) -> ee.Feature:
    site_id = ee.String(feature.get("LTER")).cat("___").cat(
        ee.String(feature.get("Stream_Name"))
    )
    return feature.set("site_id", site_id)


def extract_histograms(
    feature_collection: ee.FeatureCollection,
    mosaic: ee.Image,
    scale_m: int,
    max_pixels: float,
) -> list[dict]:
    list_len = feature_collection.size().getInfo()
    features = feature_collection.toList(count=list_len)
    band_names = mosaic.bandNames().getInfo()
    results = []

    for i in range(list_len):
        feature = ee.Feature(features.get(i))
        for band in band_names:
            pixel_count = mosaic.select(band).reduceRegion(
                reducer=ee.Reducer.frequencyHistogram(),
                geometry=feature.geometry(),
                scale=scale_m,
                maxPixels=max_pixels,
            ).get(band)
            result = feature.set(f"lcPixelCount_{band}", pixel_count).getInfo()
            results.append(result)

    return results


def histogram_results_to_frame(results: list[dict]) -> pd.DataFrame:
    rows = []
    for feature in results:
        props = feature["properties"]
        site_id = props["site_id"]
        lter = props["LTER"]
        stream_name = props["Stream_Name"]
        for key, value in props.items():
            if key.startswith("lcPixelCount_") and isinstance(value, dict):
                for lc_id, count in value.items():
                    rows.append(
                        {
                            "site_id": site_id,
                            "LTER": lter,
                            "Stream_Name": stream_name,
                            "Band": key,
                            "LC_ID": str(lc_id),
                            "Pixel_Count": count,
                        }
                    )

    return pd.DataFrame(rows)


def finalize_lulc_frame(
    df: pd.DataFrame,
    band_to_year: dict[str, int],
) -> pd.DataFrame:
    df = df.copy()
    df["Year"] = df["Band"].map(band_to_year)
    df["LandClass"] = df["LC_ID"].map(CLASS_TO_LANDTYPE)
    df["Area_m2"] = df["Pixel_Count"] * 1000000
    return df[
        [
            "site_id",
            "LTER",
            "Stream_Name",
            "Year",
            "LC_ID",
            "LandClass",
            "Pixel_Count",
            "Area_m2",
        ]
    ]


def main() -> None:
    authenticate_ee()

    target_sites = load_target_sites(TARGET_SITES_CSV)
    basins = ee.FeatureCollection(GEE_ASSET_PATH)
    filtered = basins.filter(build_site_filter(target_sites)).map(add_site_id)

    annual = ee.ImageCollection(
        "projects/sat-io/open-datasets/GLC-FCS30D/annual"
    ).mosaic()
    five_year = ee.ImageCollection(
        "projects/sat-io/open-datasets/GLC-FCS30D/five-years-map"
    ).mosaic()

    annual_results = extract_histograms(
        filtered,
        annual,
        scale_m=REDUCTION_SCALE_M,
        max_pixels=ANNUAL_MAX_PIXELS,
    )
    five_year_results = extract_histograms(
        filtered,
        five_year,
        scale_m=REDUCTION_SCALE_M,
        max_pixels=FIVE_YEAR_MAX_PIXELS,
    )

    annual_df = finalize_lulc_frame(
        histogram_results_to_frame(annual_results),
        ANNUAL_BAND_TO_YEAR,
    )
    five_year_df = finalize_lulc_frame(
        histogram_results_to_frame(five_year_results),
        FIVE_YEAR_BAND_TO_YEAR,
    )

    output_dir = Path(DRIVE_OUTPUT_DIR)
    output_dir.mkdir(parents=True, exist_ok=True)
    annual_df.to_csv(output_dir / ANNUAL_OUTPUT_NAME, index=False)
    five_year_df.to_csv(output_dir / FIVE_YEAR_OUTPUT_NAME, index=False)


if __name__ == "__main__":
    main()
