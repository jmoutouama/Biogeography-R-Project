/*
Function of the script:
extract area of tree cover surrounding pts from dynamic world for the 12 pts, 3 yrs
Author: Wenxin Yang
Date: 12/14/2023
*/

// import dataset
var pts = ee.FeatureCollection("users/wenxinyang/pts_project"),
    pts_buff = ee.FeatureCollection("users/wenxinyang/pts_buff_1km");
print(pts_buff);
// buffered point dataset as feature collection
var area = pts_buff.first().geometry().area();
print(area);

// convert to raster to make it easier to compute
var ras_buff = ee.Image().int().paint(pts_buff, 'id').rename("pointID");

var dwVisParams = {
  min: 0,
  max: 8,
  palette: [
    '#419BDF', '#397D49', '#88B053', '#7A87C6', '#E49635', '#DFC35A',
    '#C4281B', '#A59B8F', '#B39FE1'
  ]
};

// ================= get dw value for a particular year - function ===============

function getYrDW(y){

  // specify start and end years
  var startDate = ee.Date.fromYMD(y, 1, 1);
  var endDate = ee.Date.fromYMD(y, 12, 31);

  // get initial composite
  var dw = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1')
              .filterDate(startDate, endDate)
              .filterBounds(pts_buff);

  // get band for classification type
  var classification = dw.select('label');
  // reduce to highest prob land cover type
  var dwComposite = classification.reduce(ee.Reducer.mode());

  // get forest cover
  var forest = dwComposite.eq(1).rename('forest');

  // compute tree cover within the buffer region
  var sumyr = forest.reduceRegions({
  collection: pts_buff,
  reducer: ee.Reducer.sum(),
  scale: 30,
  });

  print(sumyr);

  var taskname = "export".concat(y.toString());
  var filename = "pct_tree_".concat(y.toString());
  // var filename = ee.String("pct_tree_").cat(y.toString());

// Export features, specifying corresponding names.
  Export.table.toDrive(sumyr,
  taskname, //my task
  "GEE_Folder", //my export folder
  filename,  //file name
  "CSV");

}

// ================= call function ===============
getYrDW(2019);
getYrDW(2020);
getYrDW(2021);
