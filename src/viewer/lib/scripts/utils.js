import { color } from './dat.gui.module.js';
import * as THREE from './three.module.js';


export function valDict(atoms) {
    var dic = {}
    atoms.forEach(atomName => {
        dic[atomName] = [];
    });
    return dic;
}

export function udpateEval(evalDic, atomList) {
    for (var atomName in evalDic) {
        evalDic[atomName].push( atomList.includes(atomName) );
    }
}

export function udpateEvalExt(evalDic, atomsData, i) {
    for (var atomName in evalDic) {
        evalDic[atomName].push( atomsData[atomName][i] );
    }
}

export function determineEvalColor(arrayEvalColor, amount, simplex, params) {
    if (simplex["atoms"].includes( params['propertyName'] )) {
        for (let i = 0; i < amount; i++) { arrayEvalColor.push( new THREE.Color(params['truePropertyColor']) ); } 
    } else {
        for (let i = 0; i < amount; i++) { arrayEvalColor.push( new THREE.Color(params['falsePropertyColor']) ); } 
    }
}



export function addColorAttribute(bufferGeometry) {
    const colorBuffer = new THREE.BufferAttribute(new Float32Array(bufferGeometry.getAttribute('position').count * 3), 3);
    bufferGeometry.setAttribute( 'color', colorBuffer );
}


function hexToRGB(color) {
    return [ Math.floor(color /65536)/255, Math.floor(color /256)%256/255, Math.floor(color %256)/255 ];
}

export function setColorBase(object, color) {
    var colorRGB  = hexToRGB(color);
    var colorBuffer = object.geometry.getAttribute('color');
    for (let i = 0; i < colorBuffer.array.length; i++) {
        colorBuffer.array[i] = colorRGB[i%3];
    }
    colorBuffer.needsUpdate = true;
}

export function setColorFromArray(object, colorArray, numberOfEntriesPerUnit) {
    var colorBuffer = object.geometry.getAttribute('color');
    for (let i = 0; i < colorBuffer.array.length; i++) {
        var simplexIndex = Math.floor(i / (3 * numberOfEntriesPerUnit));
        colorBuffer.array[i] = hexToRGB(colorArray[simplexIndex])[i % 3];
    }
    colorBuffer.needsUpdate = true;
}

export function setColorEval(object, valuation, verticesPerSimplex, trueColor, falseColor) {
    // This function presupposes that geomtery has already a color attribute. See addColorAttribute.
    var trueColorRGB  = hexToRGB(trueColor);
    var falseColorRGB = hexToRGB(falseColor);
    var colorBuffer = object.geometry.getAttribute('color');
    for (let simplexIndex = 0; simplexIndex < valuation.length; simplexIndex++) {
        for (let vertexIndex = 0; vertexIndex < verticesPerSimplex; vertexIndex++) {
            colorBuffer.array[ (simplexIndex * verticesPerSimplex + vertexIndex) * 3   ] = valuation[simplexIndex] ? trueColorRGB[0] : falseColorRGB[0];
            colorBuffer.array[ (simplexIndex * verticesPerSimplex + vertexIndex) * 3 +1] = valuation[simplexIndex] ? trueColorRGB[1] : falseColorRGB[1];
            colorBuffer.array[ (simplexIndex * verticesPerSimplex + vertexIndex) * 3 +2] = valuation[simplexIndex] ? trueColorRGB[2] : falseColorRGB[2];
        }
    }
    colorBuffer.needsUpdate = true;
}

export function determineSimplexColor(colorArray,colorData,colorDefault,atomsData,index) {
    if(colorData) {
        for(var key in colorData) {
            if(atomsData[key][index]) {
                colorArray.push(colorData[key]);
                // colorArray.push(0x0000ff);
                return;
            }
        }
    }
    colorArray.push(colorDefault);
}



export function setMeshBase(objects) {
    // not yet implemented
}

export function setMeshEval(atom, objects) {
    for (var key in objects) {
        objects[key].visible = false;
    };
    objects[atom].visible = true;
}


// export function updateMaterialColor(material, useVertexColor, baseColor) {
//     material.needsUpdate = true
//     material.vertexColors = useVertexColor;
//     if (useVertexColor) {
//         material.color.set( 0xffffff);
//     } else {
//         material.color.set( baseColor );
//     }
// }



export function computeResizedCoordinatesTetrahedron(tetrahedronResizedCoordinates, tetrahedronCoordinates, barycenter, sizeParameter) {
    // Compute the resized coordinates
    var smallerCoords = [
        new THREE.Vector3().lerpVectors( barycenter, tetrahedronCoordinates[0], sizeParameter ),
        new THREE.Vector3().lerpVectors( barycenter, tetrahedronCoordinates[1], sizeParameter ),
        new THREE.Vector3().lerpVectors( barycenter, tetrahedronCoordinates[2], sizeParameter ),
        new THREE.Vector3().lerpVectors( barycenter, tetrahedronCoordinates[3], sizeParameter )
    ]

    // Add the faces one at a time. Order matters!!!
    tetrahedronResizedCoordinates.push( smallerCoords[0] );
    tetrahedronResizedCoordinates.push( smallerCoords[1] );
    tetrahedronResizedCoordinates.push( smallerCoords[2] );

    tetrahedronResizedCoordinates.push( smallerCoords[0] );
    tetrahedronResizedCoordinates.push( smallerCoords[3] );
    tetrahedronResizedCoordinates.push( smallerCoords[1] );

    tetrahedronResizedCoordinates.push( smallerCoords[1] );
    tetrahedronResizedCoordinates.push( smallerCoords[3] );
    tetrahedronResizedCoordinates.push( smallerCoords[2] );

    tetrahedronResizedCoordinates.push( smallerCoords[0] );
    tetrahedronResizedCoordinates.push( smallerCoords[2] );
    tetrahedronResizedCoordinates.push( smallerCoords[3] );
}

export function resizeTetrahedrons (tetrahedronsGeometry, tetrahedronsCoordinates, tetrahedronsBarycenters, sizeParameter ) {
    const positions = tetrahedronsGeometry.attributes.position.array;
    const numberTetrahedrons = tetrahedronsBarycenters.length;
    for (let i=0; i < numberTetrahedrons; ++i ) {
        // console.log(positions);

        var updatedCoords = [
            new THREE.Vector3().lerpVectors( tetrahedronsBarycenters[i], tetrahedronsCoordinates[4*i  ], sizeParameter ),
            new THREE.Vector3().lerpVectors( tetrahedronsBarycenters[i], tetrahedronsCoordinates[4*i+1], sizeParameter ),
            new THREE.Vector3().lerpVectors( tetrahedronsBarycenters[i], tetrahedronsCoordinates[4*i+2], sizeParameter ),
            new THREE.Vector3().lerpVectors( tetrahedronsBarycenters[i], tetrahedronsCoordinates[4*i+3], sizeParameter )
        ]
        // console.log(tetrahedronsCoordinates[4*i]);

        // Same order as computeResizedCoordinatesTetrahedron function!!!
        updatedCoords[0].toArray(positions, 36*i    );
        updatedCoords[1].toArray(positions, 36*i + 3);
        updatedCoords[2].toArray(positions, 36*i + 6);

        updatedCoords[0].toArray(positions, 36*i + 9);
        updatedCoords[3].toArray(positions, 36*i +12);
        updatedCoords[1].toArray(positions, 36*i +15);

        updatedCoords[1].toArray(positions, 36*i +18);
        updatedCoords[3].toArray(positions, 36*i +21);
        updatedCoords[2].toArray(positions, 36*i +24);

        updatedCoords[0].toArray(positions, 36*i +27);
        updatedCoords[2].toArray(positions, 36*i +30);
        updatedCoords[3].toArray(positions, 36*i +33);

    }
}



