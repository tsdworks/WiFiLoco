#include <ESP8266WiFi.h>
#include <WiFiClient.h>
#include <ESP8266WebServer.h>

#define FORWARD 1
#define BACKWARD 0
#define NOR_DIR FWD
#define NOR_SPEED 100
#define MAX_SPEED 800
#define MIN_SPEED 0
#define LIGHT_OFF 0
#define LIGHT_MID 300
#define LIGHT_HIGH 600
#define PWR_MIN 0
#define PWR_MAX 100
#define ACCURATE 1
#define ACCURATE_TIME 20

enum LocoDirection {FWD, NEU, BWD};

class LocoMotor
{
private:
	u8 motor0_IN1;
	u8 motor0_IN2;
	u8 motor0_PWM;
	u8 motor1_IN1;
	u8 motor1_IN2;
	u8 motor1_PWM;
	int motor_Speed;
	LocoDirection motor_Dir;
public:
	LocoMotor(u8 m0_IN1, u8 m0_IN2, u8 m0_PWM, u8 m1_IN1, u8 m1_IN2, u8 m1_PWM)
	{
		pinMode(motor0_IN1 = m0_IN1, OUTPUT);
		pinMode(motor0_IN2 = m0_IN2, OUTPUT);
		pinMode(motor0_PWM = m0_PWM, OUTPUT);
		pinMode(motor1_IN1 = m1_IN1, OUTPUT);
		pinMode(motor1_IN2 = m1_IN2, OUTPUT);
		pinMode(motor1_PWM = m1_PWM, OUTPUT);
		motor_Dir = NOR_DIR;
		motor_Speed = MIN_SPEED;
	}

	void SetSpeed(int tarSpd)
	{
		motor_Speed = constrain(tarSpd, MIN_SPEED, MAX_SPEED);
	}

	void Stop()
	{
		SetSpeed(MIN_SPEED);
	}

	void SetDir(LocoDirection tarDir)
	{
		if (!motor_Speed)motor_Dir = tarDir;
	}

	void Process()
	{
		digitalWrite(motor0_IN1, motor_Dir == FWD ? FORWARD : BACKWARD);
		digitalWrite(motor0_IN2, !(motor_Dir == FWD ? FORWARD : BACKWARD));
		digitalWrite(motor1_IN1, !(motor_Dir == FWD ? FORWARD : BACKWARD));
		digitalWrite(motor1_IN2, motor_Dir == FWD ? FORWARD : BACKWARD);
		analogWrite(motor0_PWM, motor_Speed);
		analogWrite(motor1_PWM, motor_Speed);
	}
};

//13 FWhite 15 FRed 3 BWhite 1 BRed
class LocoLight
{
private:
	u8 fwdWhite_PWM;
	u8 fwdRed_PWM;
	u8 bwdWhite_PWM;
	u8 bwdRed_PWM;
	u8 currentWhiteLight;
	u8 currentRedLight;
public:
	LocoLight(u8 fw_PWM, u8 fr_PWM, u8 bw_PWM, u8 br_PWM)
	{
		pinMode(fwdWhite_PWM = fw_PWM, OUTPUT);
		pinMode(fwdRed_PWM = fr_PWM, OUTPUT);
		pinMode(bwdWhite_PWM = bw_PWM, OUTPUT);
		pinMode(bwdRed_PWM = br_PWM, OUTPUT);
		SetDir(FWD);
	}

	void SetDir(LocoDirection tarDir)
	{
		if (tarDir == FWD)
		{
			analogWrite(fwdRed_PWM, LIGHT_OFF);
			analogWrite(bwdWhite_PWM, LIGHT_OFF);
			currentWhiteLight = fwdWhite_PWM;
			currentRedLight = bwdRed_PWM;
		}
		else
		{
			analogWrite(bwdRed_PWM, LIGHT_OFF);
			analogWrite(fwdWhite_PWM, LIGHT_OFF);
			currentWhiteLight = bwdWhite_PWM;
			currentRedLight = fwdRed_PWM;
		}
	}

	void LightOFF()
	{
		analogWrite(currentWhiteLight, LIGHT_OFF);
		analogWrite(currentRedLight, LIGHT_OFF);
	}

	void LightMid()
	{
		analogWrite(currentWhiteLight, LIGHT_MID);
		analogWrite(currentRedLight, LIGHT_HIGH);
	}

	void LightMax()
	{
		analogWrite(currentWhiteLight, LIGHT_HIGH);
		analogWrite(currentRedLight, LIGHT_HIGH);
	}
};

LocoMotor motor(5, 4, 0, 14, 12, 2);//motor0_IN1 motor0_IN2 motor0_PWM motor1_...
LocoLight light(13, 15, 3, 1);//forward_white_pmw,forward_red_pmw,backward...

class Locomotive
{
private:
	LocoDirection loco_LastLightDir;
public:
	int loco_tarSpeed;
	int loco_curSpeed;
	LocoDirection loco_Dir;
	LocoDirection loco_LightDir;
	int loco_LightLevel;
	Locomotive()
	{
		Reset();
	}

	void Reset()
	{
		loco_curSpeed = PWR_MIN;
		loco_tarSpeed = PWR_MIN;
		loco_Dir = NEU;
		loco_LightDir = FWD;
		loco_LastLightDir = loco_LightDir;
		loco_LightLevel = LIGHT_OFF;
	}

	void SetDir(LocoDirection tarDir)
	{
		if (!loco_tarSpeed)loco_Dir = tarDir;
		if (loco_Dir == NEU)loco_curSpeed = PWR_MIN;
	}

	void Stop()
	{
		loco_Dir = NEU;
	}

	void SetSpeed(int tarSpd)
	{
		if (loco_Dir != NEU)loco_tarSpeed = constrain(tarSpd, PWR_MIN, PWR_MAX);
	}

	void SetLightDir(LocoDirection tarDir)
	{
		loco_LastLightDir = loco_LightDir;
		loco_LightDir = tarDir;
	}

	void SetLightLevel(int tarLevel)
	{
		loco_LightLevel = tarLevel;
	}

	void Process()
	{
		//set lights
		if (loco_LightDir != loco_LastLightDir)light.SetDir(loco_LightDir);
		if (loco_LightLevel == LIGHT_OFF)light.LightOFF();
		else if (loco_LightLevel == LIGHT_MID)light.LightMid();
		else light.LightMax();

		//set motors
		if (loco_Dir == NEU)
		{
			motor.Stop();
			loco_curSpeed = PWR_MIN;
			loco_tarSpeed = PWR_MIN;
		}
		else motor.SetDir(loco_Dir);
		if (loco_curSpeed < loco_tarSpeed)
		{
			loco_curSpeed += ACCURATE;
			motor.SetSpeed(map(loco_curSpeed, PWR_MIN, PWR_MAX, MIN_SPEED, MAX_SPEED));
			delay(ACCURATE_TIME);
		}
		else if (loco_curSpeed > loco_tarSpeed)
		{
			loco_curSpeed -= ACCURATE;
			motor.SetSpeed(map(loco_curSpeed, PWR_MIN, PWR_MAX, MIN_SPEED, MAX_SPEED));
			delay(ACCURATE_TIME);
		}
		motor.Process();
	}
};

//define loco
Locomotive myLoco;
//define web server
const char *wifi_SSID = "Locomotive@TSD";//define your SSID here
const char *wifi_PWD = "Locomotive@TSD";//define your AP Password here
ESP8266WebServer webService(80);

void setup()
{
	//set loco
	myLoco.Reset();

	//set WiFi
	WiFi.softAP(wifi_SSID, wifi_PWD);

	//set webservice
	webService.on("/", []() {
		webService.send(200, "text/plain", "Use TSD Locomotive Controller to control the locomotive.");
		delay(100);
	});
	webService.on("/setforward", []() {myLoco.SetDir(FWD); webService.send(200, "text/plain", "SetForward");});
	webService.on("/setneu", []() {myLoco.SetDir(NEU); webService.send(200, "text/plain", "SetNeutral");});
	webService.on("/setbackward", []() {myLoco.SetDir(BWD); webService.send(200, "text/plain", "SetBackward");});
	webService.on("/lightfoward", []() {myLoco.SetLightDir(FWD); webService.send(200, "text/plain", "SetLightForward");});
	webService.on("/lightbackward", []() {myLoco.SetLightDir(BWD); webService.send(200, "text/plain", "SetLightForward");});
	webService.on("/lightoff", []() {myLoco.SetLightLevel(LIGHT_OFF); webService.send(200, "text/plain", "LightOFF");});
	webService.on("/lightmid", []() {myLoco.SetLightLevel(LIGHT_MID); webService.send(200, "text/plain", "LightMID");});
	webService.on("/lightmax", []() {myLoco.SetLightLevel(LIGHT_HIGH); webService.send(200, "text/plain", "LightMAX");});
	webService.on("/setpower", []() {myLoco.SetSpeed(webService.arg("value").toInt()); webService.send(200, "text/plain", "SetPower");});
	webService.on("/emergency", []() {myLoco.Stop(); webService.send(200, "text/plain", "Emergency");});
	webService.on("/reset", []() {myLoco.Reset(); webService.send(200, "text/plain", "Emergency");});
	webService.begin();
}

void loop()
{
	webService.handleClient();
	myLoco.Process();
}

