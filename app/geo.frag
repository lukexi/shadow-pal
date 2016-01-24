#version 330 core

uniform vec4 uDiffuse;
uniform sampler2D uShadowMap;

in      vec3 vPosition;
in      vec3 vNormal;
in      vec2 vUV;

in      vec3 vShadowCoord;

out     vec4 fragColor;

const   vec3 lightColor = vec3(1);
const   float ambient = 0.2;
float n2rand( vec2 n );

void main() {


    // Shadowing
    float visibility = 1.0;
    if (texture(uShadowMap, vShadowCoord.xy).z < vShadowCoord.z) {
        visibility = 0.5;
    }


    vec3 lightPosition = vec3(0,1,1);
    vec4 color = uDiffuse;


    // Lighting
    //calculate normal in world coordinates
    vec3 normal = normalize(vNormal);
    // vec3 normal = vNormal;

    //calculate the location of this fragment in world coordinates
    vec3 surfacePos = vPosition;
    
    // vec4 surfaceColor = texture(materialTex, fragTexCoord);
    vec4 surfaceColor = color;
    vec3 surfaceToLight = normalize(lightPosition - surfacePos);

    // Calculate final color of the pixel, based on:
    // 1. The angle of incidence: diffuseCoefficient
    // 2. The color/intensities of the light: lightColor
    // 3. The diffuse color: surfaceColor

    float diffuseCoefficient = max(ambient, dot(normal, surfaceToLight));
    vec3 final = visibility * diffuseCoefficient * surfaceColor.rgb * lightColor;
    
    fragColor = vec4(final, uDiffuse.a);

    fragColor = texture(uShadowMap, vUV);
}


float nrand(vec2 n)
{
    return fract(sin(dot(n.xy, vec2(12.9898, 78.233)))* 43758.5453);
}

float n2rand(vec2 n)
{
    float iGlobalTime = 0;
    float t = fract( iGlobalTime );
    float nrnd0 = nrand( n + 0.07*t );
    float nrnd1 = nrand( n + 0.11*t );
    return (nrnd0+nrnd1) / 2.0;
}
