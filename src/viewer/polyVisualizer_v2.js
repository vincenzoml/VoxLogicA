import * as THREE from './lib/scripts/three.module.js';

import { OrbitControls } from './lib/scripts/OrbitControls.js';

import { GUI } from './lib/scripts/dat.gui.module.js';

import * as UTILS from './lib/scripts/utils.js';


// user interface and default parameters
var params = { 
    'showProperty': false,
    'currentProperty': "",
    'truePropertyColor': 0x00ff00,
    'falsePropertyColor': 0xff0000,
    'backgroundColor': 0xfffdd0,
    'pointsMaterialColor': 0x00ff00,
    'pointsOpacity': 1,
    'pointsOpacityYay': .9,
    'pointsOpacityNay': .1,
    'edgesMaterialColor': 0x0000ff,
    'edgesOpacity': 1,
    'edgesOpacityYay': .9,
    'edgesOpacityNay': .05,
    'trianglesMaterialColor': 0xff0000,
    'trianglesOpacity': .2,
    'trianglesOpacityYay': .5,
    'trianglesOpacityNay': .01,
    'tetrahedronsMaterialColor': 0xff0000,
    'tetrahedronsOpacity': 1,
    'tetrahedronsOpacityYay': .9,
    'tetrahedronsOpacityNay': .3,
    'tetrahedronSize': .3,
    'falseOpacityMultiplier': .1,
    'test': null
};


// setting up the scene
var gui = null;

const scene = new THREE.Scene();
const camera = new THREE.PerspectiveCamera( 60, window.innerWidth / window.innerHeight, 0.1, 1000 );

const renderer = new THREE.WebGLRenderer();
renderer.alpha = true;
renderer.setSize( window.innerWidth, window.innerHeight );
renderer.setClearColor( 0xffffff, 1 );
renderer.clear();
document.body.appendChild( renderer.domElement );
renderer.setClearColor( params['backgroundColor'], 1 );

const controls = new OrbitControls( camera, renderer.domElement );

camera.position.set(30,50,100);
controls.update();

camera.lookAt(0,0,0);
controls.update();






// Global variables:
// - modelData and atomData are the global variables containing the data of the model and atoms respectively.
var modelData   = null;
var atomsData   = null;
var colorData   = null;
// - atoms contains the names of the atomic propositions involved
var atoms = [];
// - variables containing the graphical data

var pointMaterial = null;
var pointMaterialYay = null;
var pointMaterialNay = null;
var pointColors = {};
var pointObjects = null;
var pointsCoordinates = {};

var edgeMaterial = null;
var edgeMaterialYay = null;
var edgeMaterialNay = null;
var edgeColors = {};
var edgeObjects = null;
var edgesCoordinates = {};

var triangleMaterial = null;
var triangleMaterialYay = null;
var triangleMaterialNay = null;
var triangleColors = {};
var triangleObjects = null;
var trianglesCoordinates = {};

var tetrahedronMaterial = null;
var tetrahedronMaterialYay = null;
var tetrahedronMaterialNay = null;
var tetrahedronColors = {};
var tetrahedronObjects = null;
var tetrahedronsCoordinates = {};
var tetrahedronsBarycenters = {};
var tetrahedronsResizedCoordinates = {};

var pointsLengthYay         = {};
var edgesLengthYay          = {};
var trianglesLengthYay      = {};
var tetrahedronsLengthYay   = {};

var pointEval = null;
var edgeEval = null;
var triangleEval = null;
var tetrahedronEval = null;






// Uploading and using the geometric data from the user
// First the model data
var uploadModelButton = document.getElementById('modelfileid');
var uploadAtomsButton = document.getElementById('atomsfileid');
var uploadColorsButton = document.getElementById('colorsfileid');
var uploadColorsCheckbox = document.getElementById('colorscheckid');
var submitForm = document.getElementById('submitform');

// Upload model data
uploadModelButton.addEventListener('change', function(){
    var reader = new FileReader();
    reader.onload = function(event) {
        modelData = JSON.parse(event.target.result);
        if( atomsData && (colorData || !uploadColorsCheckbox.checked) ) {
            submitForm.style.display = 'none';
            setupModel();
            setupUI();
            animate();
        }
    };
    reader.readAsText(uploadModelButton.files[0]);
});

// Upload atom data
uploadAtomsButton.addEventListener('change', function(){
    var reader = new FileReader();
    reader.onload = function(event) {
        atomsData = JSON.parse(event.target.result);
        if( modelData && (colorData || !uploadColorsCheckbox.checked) ) {
            submitForm.style.display = 'none';
            setupModel();
            setupUI();
            animate();
        }
    };
    reader.readAsText(uploadAtomsButton.files[0]);
});

// Upload color data

uploadColorsButton.addEventListener('change', function(){
    if ( uploadColorsCheckbox.checked ) {
        var reader = new FileReader();
        reader.onload = function(event) {
            colorData = JSON.parse(event.target.result);
            if( modelData && atomsData ) {
                submitForm.style.display = 'none';
                setupModel();
                setupUI();
                animate();
            }
        }
        reader.readAsText(uploadColorsButton.files[0]);
    } else {
        colorData = {}
    }
});











// Read the model and atoms data and upload them in the graphic card
function setupModel () {
    // console.log(modelData);


    // Store the atom names
    atoms = Object.keys(atomsData);
    params["currentProperty"] = atoms[0];


    // Store the coordinates of points in THREE.Vector3 structures
    const coordinatesOfPoints = [];
    modelData["coordinatesOfPoints"].forEach(coord => {
        coordinatesOfPoints.push( new THREE.Vector3( coord[0], coord[1], coord[2] ) );
    });






    pointEval = UTILS.valDict(atoms);
    edgeEval = UTILS.valDict(atoms);
    triangleEval = UTILS.valDict(atoms);
    tetrahedronEval = UTILS.valDict(atoms);

    atoms.forEach( atom => {
        pointsCoordinates[atom]                 = [];
        edgesCoordinates[atom]                  = [];
        trianglesCoordinates[atom]              = [];

        pointColors[atom]                       = [];
        edgeColors[atom]                        = [];
        triangleColors[atom]                    = [];

        tetrahedronsCoordinates[atom]           = [];
        tetrahedronsBarycenters[atom]           = [];
        tetrahedronsResizedCoordinates[atom]    = [];
        tetrahedronColors[atom]                 = [];
        
        var pointsCoordinatesNay                = [];
        var edgesCoordinatesNay                 = [];
        var trianglesCoordinatesNay             = [];

        var pointColorsNay                = [];
        var edgeColorsNay                 = [];
        var triangleColorsNay             = [];

        var tetrahedronsCoordinatesNay          = [];
        var tetrahedronsBarycentersNay          = [];
        var tetrahedronsResizedCoordinatesNay   = [];
        var tetrahedronColorsNay                = [];

        modelData["simplexes"].forEach( (simplex, i) => {
            var pointsOfSimplex = simplex["points"];

            switch (pointsOfSimplex.length) {
                case 1:
                    if(atomsData[atom][i]) {
                        pointsCoordinates[atom].push( coordinatesOfPoints[pointsOfSimplex[0]] );
                        UTILS.determineSimplexColor(pointColors[atom],colorData,params['pointsMaterialColor'],atomsData,i);
                    } else {
                        pointsCoordinatesNay.push( coordinatesOfPoints[pointsOfSimplex[0]] );
                        UTILS.determineSimplexColor(pointColorsNay,colorData,params['pointsMaterialColor'],atomsData,i);
                    };
                    // UTILS.udpateEval(pointEval, simplex["atoms"]);
                    // UTILS.udpateEvalExt(pointEval, atomsData, i);
                    pointEval[atom].push( atomsData[atom][i] );
                    break;

                case 2:
                    if(atomsData[atom][i]) {
                        edgesCoordinates[atom].push( coordinatesOfPoints[pointsOfSimplex[0]]);
                        edgesCoordinates[atom].push( coordinatesOfPoints[pointsOfSimplex[1]]);
                        UTILS.determineSimplexColor(edgeColors[atom],colorData,params['edgesMaterialColor'],atomsData,i);
                    } else {
                        edgesCoordinatesNay.push( coordinatesOfPoints[pointsOfSimplex[0]]);
                        edgesCoordinatesNay.push( coordinatesOfPoints[pointsOfSimplex[1]]);
                        UTILS.determineSimplexColor(edgeColorsNay,colorData,params['edgesMaterialColor'],atomsData,i);
                    };
                    // UTILS.udpateEval(edgeEval, simplex["atoms"]);
                    // UTILS.udpateEvalExt(edgeEval, atomsData, i);
                    edgeEval[atom].push( atomsData[atom][i] );
                    break;

                case 3:
                    if(atomsData[atom][i]) {
                        trianglesCoordinates[atom].push( coordinatesOfPoints[pointsOfSimplex[0]]);
                        trianglesCoordinates[atom].push( coordinatesOfPoints[pointsOfSimplex[1]]);
                        trianglesCoordinates[atom].push( coordinatesOfPoints[pointsOfSimplex[2]]);
                        UTILS.determineSimplexColor(triangleColors[atom],colorData,params['trianglesMaterialColor'],atomsData,i);
                    } else {
                        trianglesCoordinatesNay.push( coordinatesOfPoints[pointsOfSimplex[0]]);
                        trianglesCoordinatesNay.push( coordinatesOfPoints[pointsOfSimplex[1]]);
                        trianglesCoordinatesNay.push( coordinatesOfPoints[pointsOfSimplex[2]]);
                        UTILS.determineSimplexColor(triangleColorsNay,colorData,params['trianglesMaterialColor'],atomsData,i);
                    };
                    // UTILS.udpateEval(triangleEval, simplex["atoms"]);
                    // UTILS.udpateEvalExt(triangleEval, atomsData, i);
                    break;

                case 4:
                    // The coordinates of the tetrahedron are stored in tetrahedronCoordinates
                    var tetraCoords = [
                        coordinatesOfPoints[pointsOfSimplex[0]],
                        coordinatesOfPoints[pointsOfSimplex[1]],
                        coordinatesOfPoints[pointsOfSimplex[2]],
                        coordinatesOfPoints[pointsOfSimplex[3]]
                    ];
                    // Compute the coordinates of the barycenter of the tetrahedron
                    var barycenter = new THREE.Vector3().add(tetraCoords[0]).add(tetraCoords[1]).add(tetraCoords[2]).add(tetraCoords[3]).divideScalar(4);

                    if(atomsData[atom][i]) {
                        tetraCoords.forEach(c => {
                            tetrahedronsCoordinates[atom].push( c );
                        });

                        tetrahedronsBarycenters[atom].push( barycenter );
                        // Compute the resized coordinates
                        UTILS.computeResizedCoordinatesTetrahedron( tetrahedronsResizedCoordinates[atom], tetraCoords, barycenter, params["tetrahedronSize"] );
                        
                        UTILS.determineSimplexColor(tetrahedronColors[atom],colorData,params['tetrahedronsMaterialColor'],atomsData,i);
                    } else {
                        tetraCoords.forEach(c => {
                            tetrahedronsCoordinatesNay.push( c );
                        });

                        tetrahedronsBarycentersNay.push( barycenter );
                        // Compute the resized coordinates
                        UTILS.computeResizedCoordinatesTetrahedron( tetrahedronsResizedCoordinatesNay, tetraCoords, barycenter, params["tetrahedronSize"] );

                        UTILS.determineSimplexColor(tetrahedronColorsNay,colorData,params['tetrahedronsMaterialColor'],atomsData,i);
                    };
                    
                    // UTILS.udpateEval(tetrahedronEval, simplex["atoms"]);
                    // UTILS.udpateEvalExt(tetrahedronEval, atomsData, i);
                    tetrahedronEval[atom].push( atomsData[atom][i] );
                    break;
            
                default:
                    break;
            }
        });

        pointsLengthYay[atom]       = pointsCoordinates[atom].length;
        pointsCoordinates[atom]     = pointsCoordinates[atom].concat(pointsCoordinatesNay);
        pointColors[atom]           = pointColors[atom].concat(pointColorsNay);
        pointEval[atom]             = []
        for ( var i = 0 ; i < pointsLengthYay[atom]; i++ ) {
            pointEval[atom].push( true );
        }
        for ( var i = pointsLengthYay[atom] ; i < pointsCoordinates[atom]; i++ ) {
            pointEval[atom].push( false );
        }

        edgesLengthYay[atom]        = edgesCoordinates[atom].length;
        edgesCoordinates[atom]      = edgesCoordinates[atom].concat(edgesCoordinatesNay);
        edgeColors[atom]            = edgeColors[atom].concat(edgeColorsNay);
        edgeEval[atom]              = [];
        for ( var i = 0 ; i < edgesLengthYay[atom]/2; i++ ) {
            edgeEval[atom].push( true );
        }
        for ( var i = edgesLengthYay[atom]/2 ; i < edgesCoordinates[atom]/2; i++ ) {
            edgeEval[atom].push( false );
        }

        trianglesLengthYay[atom]    = trianglesCoordinates[atom].length;
        trianglesCoordinates[atom]  = trianglesCoordinates[atom].concat(trianglesCoordinatesNay);
        triangleColors[atom]        = triangleColors[atom].concat(triangleColorsNay);
        triangleEval[atom] = [];
        for ( var i = 0 ; i < trianglesLengthYay[atom]/3; i++ ) {
            triangleEval[atom].push( true );
        }
        for ( var i = trianglesLengthYay[atom]/3 ; i < trianglesCoordinates[atom]/3; i++ ) {
            triangleEval[atom].push( false );
        }

        tetrahedronsLengthYay[atom]             = tetrahedronsResizedCoordinates[atom].length;
        tetrahedronsCoordinates[atom]           = tetrahedronsCoordinates[atom].concat(tetrahedronsCoordinatesNay);
        tetrahedronsBarycenters[atom]           = tetrahedronsBarycenters[atom].concat(tetrahedronsBarycentersNay);
        tetrahedronsResizedCoordinates[atom]    = tetrahedronsResizedCoordinates[atom].concat(tetrahedronsResizedCoordinatesNay);
        tetrahedronColors[atom]                 = tetrahedronColors[atom].concat(tetrahedronColorsNay);
        tetrahedronEval[atom] = [];
        for ( var i = 0 ; i < tetrahedronsLengthYay[atom]/4; i++ ) {
            tetrahedronEval[atom].push( true );
        }
        for ( var i = tetrahedronsLengthYay[atom]/4 ; i < tetrahedronsCoordinates[atom]/4; i++ ) {
            tetrahedronEval[atom].push( false );
        }
    });
    // console.log( edgesCoordinates )


    // draw points
    pointMaterial = new THREE.PointsMaterial( {
        vertexColors: THREE.VertexColors,
        color: 0xffffff,
        opacity: params['pointsOpacity'],
        transparent: true
    } );
    pointMaterialYay = new THREE.PointsMaterial( {
        vertexColors: THREE.VertexColors,
        opacity: 1,
        transparent: true
    } );
    pointMaterialNay = new THREE.PointsMaterial( {
        vertexColors: THREE.VertexColors,
        opacity: .1,
        transparent: true
    } );
    pointObjects = {}
    // Add base point geometry
    const pointBufferGeometry = new THREE.BufferGeometry().setFromPoints( pointsCoordinates[params['currentProperty']] );
    pointBufferGeometry.addGroup( 0, pointsCoordinates[params['currentProperty']].length , 0 );
    UTILS.addColorAttribute(pointBufferGeometry);
    pointObjects['BASE_GEOMETRY'] = new THREE.Points( pointBufferGeometry, [pointMaterial,pointMaterialYay,pointMaterialNay] );
    UTILS.setColorFromArray(pointObjects['BASE_GEOMETRY'], pointColors[params['currentProperty']], 1);
    pointObjects['BASE_GEOMETRY'].visible = true;
    scene.add( pointObjects['BASE_GEOMETRY'] );
    // Add point geometry for each atom
    atoms.forEach( atom => {
        const pointBufferGeometry = new THREE.BufferGeometry().setFromPoints( pointsCoordinates[atom] );
        pointBufferGeometry.addGroup( 0, pointsLengthYay[atom] , 1 );
        pointBufferGeometry.addGroup( pointsLengthYay[atom], pointsCoordinates[atom].length - pointsLengthYay[atom] , 2 );
        UTILS.addColorAttribute(pointBufferGeometry);
        pointObjects[atom] = new THREE.Points( pointBufferGeometry, [pointMaterial,pointMaterialYay,pointMaterialNay] );
        UTILS.setColorFromArray(pointObjects[atom], pointColors[atom], 1);
        pointObjects[atom].visible = false;
        scene.add( pointObjects[atom] );
    } );
    // // Enable base point geometry
    // pointObjects['BASE_GEOMETRY'].visible = true;


    // draw edges
    edgeMaterial = new THREE.LineBasicMaterial( {
        vertexColors: THREE.VertexColors,
        opacity: params['edgesOpacity'],
        transparent: true
    } );
    edgeMaterialYay = new THREE.LineBasicMaterial( {
        vertexColors: THREE.VertexColors,
        opacity: .9,
        transparent: true
    } );
    edgeMaterialNay = new THREE.LineBasicMaterial( {
        vertexColors: THREE.VertexColors,
        opacity: .1,
        transparent: true
    } );
    edgeObjects = {}
    // Add base edge geometry
    const edgeBufferGeometry = new THREE.BufferGeometry().setFromPoints( edgesCoordinates[params['currentProperty']] );
    edgeBufferGeometry.addGroup( 0, edgesCoordinates[params['currentProperty']].length , 0 );
    UTILS.addColorAttribute(edgeBufferGeometry);
    edgeObjects['BASE_GEOMETRY'] = new THREE.LineSegments( edgeBufferGeometry, [edgeMaterial, edgeMaterialYay, edgeMaterialNay] );
    UTILS.setColorFromArray(edgeObjects['BASE_GEOMETRY'], edgeColors[params['currentProperty']], 2);
    edgeObjects['BASE_GEOMETRY'].visible = true;
    scene.add( edgeObjects['BASE_GEOMETRY'] );
    // Add edge geometry for each atom
    atoms.forEach( atom => {
        const edgeBufferGeometry = new THREE.BufferGeometry().setFromPoints( edgesCoordinates[atom] );
        edgeBufferGeometry.addGroup( 0, edgesLengthYay[atom] , 1 );
        edgeBufferGeometry.addGroup( edgesLengthYay[atom], edgesCoordinates[atom].length - edgesLengthYay[atom] , 2 );
        UTILS.addColorAttribute(edgeBufferGeometry);
        edgeObjects[atom] = new THREE.LineSegments( edgeBufferGeometry, [edgeMaterial, edgeMaterialYay, edgeMaterialNay] );
        // UTILS.setColorBase(edgeObjects[atom], params['edgesMaterialColor']);
        UTILS.setColorFromArray(edgeObjects[atom], edgeColors[atom], 2);
        edgeObjects[atom].visible = false;
        scene.add( edgeObjects[atom] );
    } );


    // draw triangles
    triangleMaterial = new THREE.MeshBasicMaterial( {
        vertexColors: THREE.VertexColors,
        opacity: params['trianglesOpacity'],
        transparent: true,
        side: THREE.DoubleSide
    } );
    triangleMaterialYay = new THREE.MeshBasicMaterial( {
        vertexColors: THREE.VertexColors,
        color: 0xffffff,
        opacity: .5,
        transparent: true,
        side: THREE.DoubleSide
    } );
    triangleMaterialNay = new THREE.MeshBasicMaterial( {
        vertexColors: THREE.VertexColors,
        opacity: .01,
        transparent: true,
        side: THREE.DoubleSide
    } );
    triangleObjects = {}
    // Add base triangle geometry
    const triangleBufferGeometry = new THREE.BufferGeometry().setFromPoints( trianglesCoordinates[params['currentProperty']] );
    triangleBufferGeometry.addGroup( 0, trianglesCoordinates[params['currentProperty']].length, 0 );
    UTILS.addColorAttribute(triangleBufferGeometry);
    triangleObjects['BASE_GEOMETRY'] = new THREE.Mesh( triangleBufferGeometry, [triangleMaterial,triangleMaterialYay,triangleMaterialNay] );
    UTILS.setColorFromArray(triangleObjects['BASE_GEOMETRY'], triangleColors[params['currentProperty']], 3);
    triangleObjects['BASE_GEOMETRY'].visible = true;
    scene.add( triangleObjects['BASE_GEOMETRY'] );
    // Add triangle geometry for each atom
    atoms.forEach( atom => {
        const triangleBufferGeometry = new THREE.BufferGeometry().setFromPoints( trianglesCoordinates[atom] );
        triangleBufferGeometry.addGroup( 0, trianglesLengthYay[atom] , 1 );
        triangleBufferGeometry.addGroup( trianglesLengthYay[atom], trianglesCoordinates[atom].length - trianglesLengthYay[atom] , 2 );
        UTILS.addColorAttribute(triangleBufferGeometry);
        triangleObjects[atom] = new THREE.Mesh( triangleBufferGeometry, [triangleMaterial,triangleMaterialYay,triangleMaterialNay] );
        // UTILS.setColorBase(triangleObjects[atom], params['trianglesMaterialColor']);
        UTILS.setColorFromArray(triangleObjects[atom], triangleColors[atom], 3);
        triangleObjects[atom].visible = false;
        scene.add( triangleObjects[atom] );
    } );


    // draw tetrahedrons
    tetrahedronMaterial = new THREE.MeshBasicMaterial( {
        vertexColors: THREE.VertexColors,
        opacity: .1,
        transparent: false,
        side: THREE.DoubleSide
    } );
    tetrahedronMaterialYay = new THREE.MeshBasicMaterial( {
        vertexColors: THREE.VertexColors,
        opacity: .9,
        transparent: false,
        side: THREE.DoubleSide
    } );
    tetrahedronMaterialNay = new THREE.MeshBasicMaterial( {
        vertexColors: THREE.VertexColors,
        opacity: .3,
        transparent: false,
        side: THREE.DoubleSide
    } );
    tetrahedronObjects = {}
    // Add base tetrahedron geometry
    const tetrahedronsBufferGeometry = new THREE.BufferGeometry().setFromPoints( tetrahedronsResizedCoordinates[params['currentProperty']] );
    tetrahedronsBufferGeometry.addGroup( 0, tetrahedronsResizedCoordinates[params['currentProperty']].length, 0 );
    UTILS.addColorAttribute(tetrahedronsBufferGeometry);
    tetrahedronObjects['BASE_GEOMETRY'] = new THREE.Mesh( tetrahedronsBufferGeometry, [tetrahedronMaterial, tetrahedronMaterialYay, tetrahedronMaterialNay] );
    UTILS.setColorFromArray(tetrahedronObjects['BASE_GEOMETRY'], tetrahedronColors[params['currentProperty']], 12);
    tetrahedronObjects['BASE_GEOMETRY'].visible = true;
    scene.add( tetrahedronObjects['BASE_GEOMETRY'] );
    // Add tetrahedron geometry for each atom
    atoms.forEach( atom => {
        const tetrahedronsBufferGeometry = new THREE.BufferGeometry().setFromPoints( tetrahedronsResizedCoordinates[atom] );
        tetrahedronsBufferGeometry.addGroup( 0, tetrahedronsLengthYay[atom] , 1 );
        // tetrahedronsBufferGeometry.addGroup( tetrahedronsLengthYay[atom], tetrahedronsResizedCoordinates[atom].length - tetrahedronsLengthYay[atom] , 2 );
        UTILS.addColorAttribute(tetrahedronsBufferGeometry);
        tetrahedronObjects[atom] = new THREE.Mesh( tetrahedronsBufferGeometry, [tetrahedronMaterial, tetrahedronMaterialYay, tetrahedronMaterialNay] );
        UTILS.setColorFromArray(tetrahedronObjects[atom], tetrahedronColors[atom], 12);
        tetrahedronObjects[atom].visible = false;
        scene.add( tetrahedronObjects[atom] );
    } );



}





// Handling events for the UI
function showPropertyChanged() {
    // change the colors of the objects in the scene
    if ( params['showProperty'] ) {
        UTILS.setMeshEval(params['currentProperty'],pointObjects);
        UTILS.setMeshEval(params['currentProperty'],edgeObjects);
        UTILS.setMeshEval(params['currentProperty'],triangleObjects);
        UTILS.setMeshEval(params['currentProperty'],tetrahedronObjects);
    } else {
        UTILS.setMeshEval('BASE_GEOMETRY',pointObjects);
        UTILS.setMeshEval('BASE_GEOMETRY',edgeObjects);
        UTILS.setMeshEval('BASE_GEOMETRY',triangleObjects);
        UTILS.setMeshEval('BASE_GEOMETRY',tetrahedronObjects);
    }
}

function currentPropertyChanged() {
    // change the current property
    if ( params['showProperty'] ) {
        UTILS.setMeshEval(params['currentProperty'],pointObjects);
        UTILS.setMeshEval(params['currentProperty'],edgeObjects);
        UTILS.setMeshEval(params['currentProperty'],triangleObjects);
        UTILS.setMeshEval(params['currentProperty'],tetrahedronObjects);
    }
}

function backgroundColorChanged() {
    // change the background color
    renderer.setClearColor( params['backgroundColor'], 1 );
}




function materialOpacityChanged(material, opacity) {
    // change opacity of material
    material.opacity = opacity;
}

function tetrahedronSizeChanged() {
    // change tetrahedra geometry
    atoms.forEach( atom => {
        UTILS.resizeTetrahedrons( tetrahedronObjects[atom].geometry, tetrahedronsCoordinates[atom], tetrahedronsBarycenters[atom], params["tetrahedronSize"]);
        tetrahedronObjects[atom].geometry.attributes.position.needsUpdate = true;
    } );
}





// Setup the UI
function setupUI () {
    gui = new GUI();
    const folderProperty = gui.addFolder( 'Property' );
    folderProperty.add( params, 'showProperty' ).name( 'Show Property' ).onChange(showPropertyChanged);
    folderProperty.add( params, 'currentProperty', atoms ).name( 'Property' ).onChange(currentPropertyChanged);
    const folderAppearance = gui.addFolder( 'Appearance' );
        const folderSceneAppearance = folderAppearance.addFolder( 'Scene' );
        folderSceneAppearance.addColor( params, 'backgroundColor' ).name('Color').onChange(backgroundColorChanged);
        const folderPointAppearance = folderAppearance.addFolder( 'Points' );
        // folderPointAppearance.addColor( params, 'pointsMaterialColor' ).name('Color').onFinishChange( () => objectColorChanged(pointObject, params['pointsMaterialColor']));
        folderPointAppearance.add( params, 'pointsOpacity'   , 0, 1 ).name( 'Opacity Base'     ).onChange( () => materialOpacityChanged(pointMaterial,    params['pointsOpacity'   ]) );
        folderPointAppearance.add( params, 'pointsOpacityYay', 0, 1 ).name( 'Opacity Property' ).onChange( () => materialOpacityChanged(pointMaterialYay, params['pointsOpacityYay']) );
        folderPointAppearance.add( params, 'pointsOpacityNay', 0, 1 ).name( 'Opacity Complement Property' ).onChange( () => materialOpacityChanged(pointMaterialNay, params['pointsOpacityNay']) );
        const folderEdgeAppearance = folderAppearance.addFolder( 'Edges' );
        // folderEdgeAppearance.addColor( params, 'edgesMaterialColor' ).name('Color').onFinishChange( () => objectColorChanged(edgeObject, params['edgesMaterialColor']));
        folderEdgeAppearance.add( params, 'edgesOpacity', 0, 1    ).name( 'Opacity Base'     ).onChange( () => materialOpacityChanged(edgeMaterial   , params['edgesOpacity']) );
        folderEdgeAppearance.add( params, 'edgesOpacityYay', 0, 1 ).name( 'Opacity Property' ).onChange( () => materialOpacityChanged(edgeMaterialYay, params['edgesOpacityYay']) );
        folderEdgeAppearance.add( params, 'edgesOpacityNay', 0, 1 ).name( 'Opacity Complement Property' ).onChange( () => materialOpacityChanged(edgeMaterialNay, params['edgesOpacityNay']) );
        const folderTriangleAppearance = folderAppearance.addFolder( 'Triangles' );
        // folderTriangleAppearance.addColor( params, 'trianglesMaterialColor' ).name('Color').onFinishChange( () => objectColorChanged(triangleObjects[params['currentProperty']], params['trianglesMaterialColor']));
        folderTriangleAppearance.add( params, 'trianglesOpacity'   , 0, 1 ).name( 'Opacity Base'     ).onChange( () => materialOpacityChanged(triangleMaterial   , params['trianglesOpacity']) );
        folderTriangleAppearance.add( params, 'trianglesOpacityYay', 0, 1 ).name( 'Opacity Property' ).onChange( () => materialOpacityChanged(triangleMaterialYay, params['trianglesOpacityYay']) );
        folderTriangleAppearance.add( params, 'trianglesOpacityNay', 0, 1 ).name( 'Opacity Complement Property' ).onChange( () => materialOpacityChanged(triangleMaterialNay, params['trianglesOpacityNay']) );
        const folderTetrahedronAppearance = folderAppearance.addFolder( 'Tetrahedrons' );
        // folderTetrahedronAppearance.addColor( params, 'tetrahedronsMaterialColor' ).name('Color').onFinishChange( () => objectColorChanged(tetrahedronObject, params['tetrahedronsMaterialColor']));
        // folderTetrahedronAppearance.add( params, 'tetrahedronsOpacity', 0, 1 ).name( 'Opacity' ).onChange( () => materialOpacityChanged(tetrahedronMaterial, params['tetrahedronsOpacity']) );
        folderTetrahedronAppearance.add( params, 'tetrahedronSize', 0, 1 ).name( 'Size' ).onChange(tetrahedronSizeChanged);
}

// gui.add( params, 'test', { a: 'A', b: 'B', c: 'C' } ).name('Something').onChange( function (){console.log( params['test'] );} );



function animate() {

    requestAnimationFrame( animate );

    if (modelData && atomsData) {
        // required if controls.enableDamping or controls.autoRotate are set to true
        controls.update();

        renderer.render( scene, camera );
    }

}
// animate();